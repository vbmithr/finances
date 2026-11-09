open Base

(* Only records and CSV concepts belong here. Provider meaning belongs in Lua. *)
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

type t =
  { source : string
  ; institution : string
  ; identity : string
  ; header : string list
  ; separator : char
  }

let initialize source =
  let state = I.mk () in
  I.register_globals
    [ ( "array"
      , V.caml_func (function
          | [] -> [ array [] ]
          | _ -> failwith "array expects no arguments") )
    ; "sign", V.caml_func sign
    ; "decimal", V.caml_func decimal
    ; "utc_datetime", V.caml_func utc_datetime
    ; "yes", L.Userdata (Userdata.Boolean true)
    ; "no", L.Userdata (Userdata.Boolean false)
    ]
    state;
  ignore (I.dostring ~file:"converter.lua" state source : V.value list);
  let config = I.getglobal state (L.String "config") |> lua_table in
  let institution = get_string config "institution" in
  let identity = get_string config "identity" in
  let converter = get_string config "converter" in
  let version = get_string config "version" in
  let account = get config "account" |> lua_table in
  let expected = get config "header" |> lua_table |> lua_list |> List.map ~f:string in
  let separator =
    match get config "separator" with
    | L.Nil -> ','
    | L.String s when String.length s = 1 -> s.[0]
    | _ -> failwith "separator must be one character"
  in
  let apply =
    match I.getglobal state (L.String "convert") with
    | L.Function (_, apply) -> apply
    | _ -> failwith "script must define convert"
  in
  state, institution, identity, converter, version, account, expected, separator, apply
;;

let create ~institution source =
  Or_error.try_with (fun () ->
    let _, declared, identity, _, _, _, header, separator, _ = initialize source in
    if not (String.equal institution declared)
    then fail "Lua converter belongs to institution %s, expected %s" declared institution;
    if not (List.mem [ "source"; "inventory" ] identity ~equal:String.equal)
    then fail "unknown identity: %s" identity;
    if List.is_empty header || List.contains_dup header ~compare:String.compare
    then failwith "Lua converters require a nonempty header with distinct names";
    { source; institution; identity; header; separator })
;;

let of_string source =
  Or_error.try_with (fun () ->
    let _, institution, _, _, _, _, _, _, _ = initialize source in
    create ~institution source |> Or_error.ok_exn)
;;

let recognizes t contents =
  match
    Or_error.try_with (fun () ->
      Csv.next (Csv_reader.channel ~separator:t.separator contents))
  with
  | Ok header -> List.equal String.equal header t.header
  | Error _ -> false
;;

let convert t ~name contents =
  Or_error.try_with (fun () ->
    let _, institution, identity, converter, version, account, expected, separator, apply =
      initialize t.source
    in
    Csv_reader.check_utf8 ~name:institution contents;
    let lines = Csv.input_all (Csv_reader.channel ~separator contents) in
    let header, rows =
      match lines with
      | [] -> failwith "the CSV has no header"
      | h :: rows -> h, rows
    in
    if not (List.equal String.equal header expected)
    then failwith "CSV header does not match script";
    let records =
      List.filter_mapi rows ~f:(fun i cells ->
        if List.for_all cells ~f:(fun cell -> String.is_empty (String.strip cell))
        then None
        else if List.length cells <> List.length header
        then fail "row %d has the wrong number of columns" (i + 1)
        else (
          let raw = List.zip_exn header cells in
          let normalized =
            List.map raw ~f:(fun (key, value) -> key, String.strip value)
          in
          let fields =
            raw
            |> List.filter ~f:(fun (_, v) -> not (String.is_empty (String.strip v)))
            |> List.map ~f:(fun (k, v) -> array [ L.String k; L.String v ])
            |> array
          in
          let hash =
            Digestif.SHA256.(
              digest_string (Json.encode (Jsont.list Jsont.string) cells) |> to_hex)
          in
          let context =
            table [ "row_hash", L.String ("row-sha256-" ^ hash); "fields", fields ]
          in
          try
            match apply [ string_table normalized; string_table raw; context ] with
            | [ L.Nil ] -> failwith "skipping rows in Lua converters is not supported yet"
            | [ (L.Table _ as value) ] -> Some (json value)
            | _ -> failwith "convert must return a record table or nil"
          with
          | exn -> fail "row %d: %s" (i + 1) (Exn.to_string exn)))
    in
    let sha256 = Digestif.SHA256.(digest_string contents |> to_hex) in
    let object_line fields =
      "{"
      ^ String.concat
          ~sep:","
          (List.map fields ~f:(fun (k, v) -> json_string k ^ ":" ^ v))
      ^ "}\n"
    in
    let header =
      object_line
        [ "kind", json_string "header"
        ; "format", json_string "fin-records"
        ; "version", "1"
        ; "institution", json_string institution
        ; "identity", json_string identity
        ; ( "converter"
          , String.strip
              (object_line
                 [ "name", json_string converter; "version", json_string version ]) )
        ]
    in
    let source =
      object_line
        [ "kind", json_string "source"
        ; "name", json_string name
        ; "sha256", json_string sha256
        ]
    in
    let account_fields =
      V.Luahash.fold
        (fun k v acc ->
           match k with
           | L.String s -> (s, json v) :: acc
           | _ -> failwith "account keys must be strings")
        account
        []
    in
    let accounts =
      if List.is_empty records
      then ""
      else object_line (("kind", json_string "account") :: account_fields)
    in
    let text =
      header
      ^ source
      ^ accounts
      ^ String.concat ~sep:"" (List.map records ~f:(fun r -> r ^ "\n"))
      ^ object_line
          [ "kind", json_string "end"; "records", Int.to_string (List.length records) ]
    in
    let documents = Records.of_string text |> Or_error.ok_exn in
    match documents with
    | [ document ] -> document
    | _ -> failwith "expected one records section")
;;

let source t = t.source

let overlap t : Adapter.overlap =
  if String.equal t.identity "source" then Source_ids else Day_inventory
;;
