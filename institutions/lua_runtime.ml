open Base

(* Shared Lua values and helpers; provider interpretation belongs in scripts. *)
module Userdata = struct
  type 'a t =
    | Boolean of bool
    | Array_marker

  let tname = "boolean"
  let eq _ a b = Poly.equal a b

  let to_string _ = function
    | Boolean b -> Bool.to_string b
    | Array_marker -> "array"
  ;;
end

module Library = struct
  type 'a userdata' = 'a Userdata.t

  module M (C : Lua.Lib.CORE with type 'a V.userdata' = 'a userdata') = struct
    let init (_ : C.V.state) = ()
  end
end

module I = Lua.MakeInterp (Lua.Parser.MakeStandard) (Lua.MakeEval (Userdata) (Library))
module V = I.Value
module L = V.LuaValueBase

let fail fmt = Printf.ksprintf failwith fmt

(* Decimal values are strings in Lua. Never round-trip them through a float. *)
let quantity text =
  let text = String.strip text in
  let text =
    String.substr_replace_all text ~pattern:"\xc2\xa0" ~with_:""
    |> String.substr_replace_all ~pattern:"\xe2\x80\xaf" ~with_:""
    |> String.filter ~f:(Fn.non (Char.equal ' '))
  in
  if not (Re.execp (Re.compile (Re.Perl.re {|^-?[0-9]+(\.[0-9]+)?$|})) text)
  then fail "invalid quantity: %S" text;
  Quantity.of_string text
;;

let sign = function
  | [ L.String text ] ->
    (match Option.try_with (fun () -> quantity text) with
     | None -> [ L.Nil ]
     | Some value ->
       [ L.String
           (if Quantity.(value > zero)
            then "positive"
            else if Quantity.(value < zero)
            then "negative"
            else "zero")
       ])
  | _ -> failwith "sign expects one string"
;;

let decimal = function
  | [ L.String text ] -> [ L.String (Quantity.to_string (quantity text)) ]
  | _ -> failwith "decimal expects one string"
;;

let decimal_comma = function
  | [ L.String text ] ->
    if String.mem text '.' then fail "invalid quantity: %S" text;
    let text = String.substr_replace_all text ~pattern:"," ~with_:"." in
    [ L.String (Quantity.to_string (quantity text)) ]
  | _ -> failwith "decimal_comma expects one string"
;;

let matches = function
  | [ L.String value; L.String pattern ] ->
    if Re.execp (Re.Perl.compile_pat pattern) value then [ L.Number 1. ] else [ L.Nil ]
  | _ -> failwith "matches expects two strings"
;;

let valid_iban = function
  | [ L.String value ] ->
    if Result.is_ok (Iban.of_string value) then [ L.Number 1. ] else [ L.Nil ]
  | _ -> failwith "valid_iban expects one string"
;;

let subtract = function
  | [ L.String left; L.String right ] ->
    [ L.String (Quantity.to_string Quantity.(quantity left - quantity right)) ]
  | _ -> failwith "subtract expects two decimal strings"
;;

let date_of_datetime = function
  | [ L.String text ] ->
    if
      not
        (Re.execp
           (Re.Perl.compile_pat
              {|^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$|})
           text)
    then fail "invalid date: %S" text;
    [ L.String (String.prefix text 10) ]
  | _ -> failwith "date_of_datetime expects one string"
;;

let starts_with = function
  | [ L.String text; L.String prefix ] ->
    if String.is_prefix text ~prefix then [ L.Number 1. ] else [ L.Nil ]
  | _ -> failwith "starts_with expects two strings"
;;

let drop_prefix = function
  | [ L.String text; L.String prefix ] ->
    (match String.chop_prefix text ~prefix with
     | Some value -> [ L.String value ]
     | None -> [ L.Nil ])
  | _ -> failwith "drop_prefix expects two strings"
;;

let day_first = function
  | [ L.String text ] ->
    (match String.split text ~on:'-' with
     | [ day; month; year ] -> [ L.String (String.concat ~sep:"-" [ year; month; day ]) ]
     | _ -> fail "invalid date: %S" text)
  | _ -> failwith "day_first expects one string"
;;

let normalize_hex = function
  | [ L.String reference
    ; L.String description
    ; L.String prefix
    ; L.String label
    ; L.Number length
    ] ->
    if (not (Float.is_integer length)) || Float.(length <= 0.) || Float.(length > 4096.)
    then failwith "normalize_hex length must be a positive integer";
    [ L.String
        (Reference_normalization.normalize_hex
           ~prefix
           ~label
           ~length:(Int.of_float length)
           ~description
           reference)
    ]
  | _ -> failwith "normalize_hex expects reference, description, prefix, label and length"
;;

let negate = function
  | [ L.String text ] -> [ L.String (Quantity.to_string (Quantity.neg (quantity text))) ]
  | _ -> failwith "negate expects one string"
;;

let utc_datetime = function
  | [ L.String text ] ->
    if
      not
        (Re.execp
           (Re.compile
              (Re.Perl.re {|^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}:[0-9]{2}|}))
           text)
    then fail "invalid date: %S" text;
    [ L.String (String.prefix text 10 ^ "T" ^ String.drop_prefix text 11 ^ "Z") ]
  | _ -> failwith "utc_datetime expects one string"
;;

let table fields = L.Table (V.Table.of_list fields)
let string_table fields = table (List.map fields ~f:(fun (k, v) -> k, L.String v))
let array_key = L.String "\000fin-array-marker"

let array values =
  let t = V.Table.create (List.length values) in
  V.Table.bind t ~key:array_key ~data:(L.Userdata Userdata.Array_marker);
  List.iteri values ~f:(fun i v ->
    V.Table.bind t ~key:(L.Number (Float.of_int (i + 1))) ~data:v);
  L.Table t
;;

let get table name = V.Table.find table ~key:(L.String name)

let string = function
  | L.String s -> s
  | _ -> failwith "expected Lua string"
;;

let lua_table = function
  | L.Table t -> t
  | _ -> failwith "expected Lua table"
;;

let get_string table name = string (get table name)
let json_string s = Json.encode Jsont.string s

(* Lua 2.5 uses tables for both objects and arrays. array() marks empty arrays. *)
let rec json value =
  match value with
  | L.String s -> json_string s
  | L.Userdata (Userdata.Boolean b) -> Bool.to_string b
  | L.Userdata Userdata.Array_marker -> failwith "invalid array marker"
  | L.Table table ->
    let is_marked_array =
      match V.Table.find table ~key:array_key with
      | L.Userdata Userdata.Array_marker -> true
      | _ -> false
    in
    let entries =
      V.Luahash.fold
        (fun k v acc -> if V.eq k array_key then acc else (k, v) :: acc)
        table
        []
    in
    let numeric =
      List.for_all entries ~f:(function
        | L.Number _, _ -> true
        | _ -> false)
    in
    if
      ((not (List.is_empty entries)) && numeric)
      || (List.is_empty entries && is_marked_array)
    then (
      let items =
        List.map entries ~f:(function
          | L.Number n, value ->
            let index = Int.of_float n in
            if Float.(n <> of_int index)
            then failwith "Lua array indices must be integers";
            index, value
          | _ -> assert false)
        |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
      in
      List.iteri items ~f:(fun i (n, _) ->
        if n <> i + 1 then failwith "Lua array must have consecutive indices");
      "[" ^ String.concat ~sep:"," (List.map items ~f:(fun (_, v) -> json v)) ^ "]")
    else (
      let fields =
        List.map entries ~f:(function
          | L.String key, value -> key, value
          | _ -> failwith "Lua object keys must be strings")
        |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
      in
      "{"
      ^ String.concat
          ~sep:","
          (List.map fields ~f:(fun (k, v) -> json_string k ^ ":" ^ json v))
      ^ "}")
  | L.Nil -> "null"
  | L.Number _ -> failwith "Lua numbers are floating point; use decimal strings"
  | L.Function _ -> failwith "functions cannot appear in records"
;;

let lua_list table =
  let rec go index acc =
    match V.Table.find table ~key:(L.Number (Float.of_int index)) with
    | L.Nil -> List.rev acc
    | value -> go (index + 1) (value :: acc)
  in
  go 1 []
;;

let load source =
  let state = I.mk () in
  I.register_globals
    [ ( "array"
      , V.caml_func (function
          | [] -> [ array [] ]
          | _ -> failwith "array expects no arguments") )
    ; "sign", V.caml_func sign
    ; "decimal", V.caml_func decimal
    ; "decimal_comma", V.caml_func decimal_comma
    ; "matches", V.caml_func matches
    ; "valid_iban", V.caml_func valid_iban
    ; ( "reject"
      , V.caml_func (function
          | [ L.String reason ] -> failwith reason
          | _ -> failwith "reject expects one string") )
    ; "negate", V.caml_func negate
    ; "subtract", V.caml_func subtract
    ; "date_of_datetime", V.caml_func date_of_datetime
    ; "day_first", V.caml_func day_first
    ; "starts_with", V.caml_func starts_with
    ; "drop_prefix", V.caml_func drop_prefix
    ; "normalize_hex", V.caml_func normalize_hex
    ; "utc_datetime", V.caml_func utc_datetime
    ; "yes", L.Userdata (Userdata.Boolean true)
    ; "no", L.Userdata (Userdata.Boolean false)
    ]
    state;
  ignore (I.dostring ~file:"converter.lua" state source : V.value list);
  state
;;
