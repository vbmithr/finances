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
    let config : Camt_profile.hex_normalization =
      { prefix; description = label; length = Int.of_float length }
    in
    [ L.String (Camt_source.normalize_hex config ~description reference) ]
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

(* Duplicate CSV names have stable aliases in Lua; original names remain in fields. *)
let column_keys header =
  let seen = Hashtbl.create (module String) in
  let keys =
    List.map header ~f:(fun name ->
      let n = 1 + Option.value (Hashtbl.find seen name) ~default:0 in
      Hashtbl.set seen ~key:name ~data:n;
      if n = 1 then name else Printf.sprintf "%s#%d" name n)
  in
  if List.contains_dup keys ~compare:String.compare
  then failwith "CSV header has ambiguous numbered column names";
  keys
;;

type t =
  { source : string
  ; institution : string
  ; identity : string
  ; header : string list
  ; separator : char
  ; tags : (string * string) list
  ; journal_description : string option
  ; conflict_columns : string list
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
    ; "normalize_hex", V.caml_func normalize_hex
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
  let account =
    match get config "account" with
    | L.Nil -> V.Table.create 0
    | value -> lua_table value
  in
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
    let state, declared, identity, _, _, _, header, separator, _ = initialize source in
    let config = I.getglobal state (L.String "config") |> lua_table in
    (match get config "account", I.getglobal state (L.String "account") with
     | L.Nil, L.Function _ | L.Table _, L.Nil -> ()
     | L.Nil, L.Nil -> failwith "provide config.account or an account function"
     | L.Table _, L.Function _ -> failwith "use config.account or account, not both"
     | _ -> failwith "invalid account configuration");
    (match get config "link_statements" with
     | L.Nil | L.Userdata (Userdata.Boolean _) -> ()
     | _ -> failwith "link_statements must be yes or no");
    (match I.getglobal state (L.String "classify") with
     | L.Nil | L.Function _ -> ()
     | _ -> failwith "classify must be a function");
    if not (String.equal institution declared)
    then fail "Lua converter belongs to institution %s, expected %s" declared institution;
    if not (List.mem [ "source"; "inventory" ] identity ~equal:String.equal)
    then fail "unknown identity: %s" identity;
    if List.is_empty header then failwith "Lua converters require a nonempty header";
    ignore (column_keys header : string list);
    if not (V.eq (get config "journal_tags") L.Nil)
    then failwith "move journal_tags into config.observation";
    let tags =
      match get config "observation" with
      | L.Nil -> []
      | L.Table observation ->
        (match get observation "journal_tags" with
         | L.Nil -> []
         | L.Table table ->
           V.Luahash.fold (fun k v acc -> (string k, string v) :: acc) table []
           |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
         | _ -> failwith "journal_tags must be a table")
      | _ -> failwith "observation must be a table"
    in
    List.iter tags ~f:(fun (_, column) ->
      if not (List.mem header column ~equal:String.equal)
      then fail "journal tag column is not in header: %s" column);
    let journal_description =
      match get config "observation" with
      | L.Table observation ->
        (match get observation "journal_description" with
         | L.Nil -> None
         | L.String column -> Some column
         | _ -> failwith "journal_description must be a column name")
      | _ -> None
    in
    Option.iter journal_description ~f:(fun column ->
      if not (List.mem header column ~equal:String.equal)
      then fail "journal description column is not in header: %s" column);
    let conflict_columns =
      match get config "observation" with
      | L.Table observation ->
        (match get observation "conflict_columns" with
         | L.Nil -> []
         | value -> lua_table value |> lua_list |> List.map ~f:string)
      | _ -> []
    in
    List.iter conflict_columns ~f:(fun column ->
      if not (List.mem header column ~equal:String.equal)
      then fail "conflict column is not in header: %s" column);
    { source
    ; institution
    ; identity
    ; header
    ; separator
    ; tags
    ; journal_description
    ; conflict_columns
    })
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

type interpreted =
  | Record of Records.record
  | Statement of Records.statement

let read_rows ~institution ~contents ~separator ~expected ~classify =
  Csv_reader.check_utf8 ~name:institution contents;
  let lines = Csv.input_all (Csv_reader.channel ~separator contents) in
  let header, rows =
    match lines with
    | [] -> failwith "the CSV has no header"
    | h :: rows -> h, rows
  in
  if not (List.equal String.equal header expected)
  then failwith "CSV header does not match script";
  let keys = column_keys header in
  (* First classify the rows, then interpret transactions before statement
       claims. This lets the script describe a file total wherever it occurs. *)
  let rows =
    List.filter_mapi rows ~f:(fun i cells ->
      if List.for_all cells ~f:(fun cell -> String.is_empty (String.strip cell))
      then None
      else if List.length cells <> List.length header
      then fail "row %d has the wrong number of columns" (i + 1)
      else (
        let raw = List.zip_exn header cells in
        let named = List.zip_exn keys cells in
        let normalized =
          List.map named ~f:(fun (key, value) -> key, String.strip value)
        in
        let kind =
          match classify with
          | None -> "record"
          | Some fn ->
            (try
               match fn [ string_table normalized ] with
               | [ L.String (("record" | "statement") as kind) ] -> kind
               | _ -> failwith "classify must return record or statement"
             with
             | exn -> fail "row %d: %s" (i + 1) (Exn.to_string exn))
        in
        Some (i, cells, raw, named, normalized, kind)))
  in
  rows
;;

let interpret_row
      ~accounts
      ~balances
      ~account_fn
      ~account
      ~apply
      ~observation
      ~link_statements
      ~sha256
      (i, cells, raw, named, normalized, kind)
  =
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
    table
      [ "row_hash", L.String ("row-sha256-" ^ hash)
      ; "source_hash_prefix", L.String (String.prefix sha256 16)
      ; "fields", fields
      ]
  in
  try
    let row_account_id =
      if String.equal kind "record"
      then (
        let row_account =
          match account_fn with
          | None -> account
          | Some fn ->
            (match fn [ string_table normalized ] with
             | [ value ] -> lua_table value
             | _ -> failwith "account must return one table")
        in
        let id = get_string row_account "id" in
        let encoded = json (L.Table row_account) in
        (match Hashtbl.find accounts id with
         | None ->
           if not (V.eq (get row_account "kind") L.Nil)
           then failwith "account must not set kind";
           let tagged =
             V.Luahash.fold
               (fun key value acc -> (string key, value) :: acc)
               row_account
               []
             |> V.Table.of_list
           in
           V.Table.bind tagged ~key:(L.String "kind") ~data:(L.String "account");
           let decoded =
             Records.account_of_line (json (L.Table tagged)) |> Or_error.ok_exn
           in
           Hashtbl.set accounts ~key:id ~data:(encoded, decoded)
         | Some (previous, _) when String.equal previous encoded -> ()
         | Some _ -> fail "conflicting accounts: %s" id);
        Some id)
      else None
    in
    let returned = apply [ string_table normalized; string_table named; context ] in
    let result, balance =
      match returned with
      | [ L.Table result ] -> result, L.Nil
      | [ L.Table result; balance ] -> result, balance
      | [ L.Nil ] -> failwith "skipping rows in Lua converters is not supported yet"
      | _ -> failwith "convert must return a record and optionally a balance"
    in
    (match balance, row_account_id with
     | L.Nil, _ -> ()
     | L.Table balance, Some id ->
       (match get balance "account" with
        | L.Nil -> V.Table.bind balance ~key:(L.String "account") ~data:(L.String id)
        | L.String declared when String.equal declared id -> ()
        | _ -> failwith "balance account does not match selected account");
       if not (String.equal (get_string balance "kind") "balance")
       then failwith "second result must be a balance table";
       balances
       := (Records.balance_of_line (json (L.Table balance)) |> Or_error.ok_exn)
          :: !balances
     | _ -> failwith "second result must be a balance table for a record");
    let default name value =
      if V.eq (get result name) L.Nil
      then V.Table.bind result ~key:(L.String name) ~data:value
    in
    (match row_account_id with
     | None -> ()
     | Some id ->
       (match get result "account" with
        | L.Nil -> default "account" (L.String id)
        | L.String declared when String.equal declared id -> ()
        | _ -> failwith "record account does not match selected account");
       if observation
       then (
         match get result "observation" with
         | L.Nil -> default "observation" (L.Userdata (Userdata.Boolean true))
         | L.Userdata (Userdata.Boolean true) -> ()
         | _ -> failwith "record contradicts observation configuration");
       let details =
         match get result "details" with
         | L.Nil ->
           let d = V.Table.create 1 in
           default "details" (L.Table d);
           d
         | value -> lua_table value
       in
       if V.eq (get details "fields") L.Nil
       then V.Table.bind details ~key:(L.String "fields") ~data:fields);
    if not (String.equal (get_string result "kind") kind)
    then failwith "returned kind does not match classification";
    if
      String.equal kind "record"
      && link_statements
      && not (V.eq (get result "statements") L.Nil)
    then failwith "record cannot set statements when link_statements is enabled";
    if String.equal kind "statement"
    then Statement (Records.statement_of_line (json (L.Table result)) |> Or_error.ok_exn)
    else Record (Records.record_of_line (json (L.Table result)) |> Or_error.ok_exn)
  with
  | exn -> fail "row %d: %s" (i + 1) (Exn.to_string exn)
;;

let convert t ~name contents =
  Or_error.try_with (fun () ->
    let ( state
        , institution
        , identity
        , converter
        , version
        , account
        , expected
        , separator
        , apply )
      =
      initialize t.source
    in
    let config = I.getglobal state (L.String "config") |> lua_table in
    let link_statements =
      match get config "link_statements" with
      | L.Nil | L.Userdata (Userdata.Boolean false) -> false
      | L.Userdata (Userdata.Boolean true) -> true
      | _ -> failwith "link_statements must be yes or no"
    in
    let classify =
      match I.getglobal state (L.String "classify") with
      | L.Nil -> None
      | L.Function (_, fn) -> Some fn
      | _ -> failwith "classify must be a function"
    in
    let account_fn =
      match I.getglobal state (L.String "account") with
      | L.Nil -> None
      | L.Function (_, fn) -> Some fn
      | _ -> failwith "account must be a function"
    in
    let observation =
      match get config "observation" with
      | L.Nil -> false
      | L.Table _ -> true
      | _ -> failwith "observation must be a table"
    in
    let balances = ref [] in
    let accounts = Hashtbl.create (module String) in
    let positions = Hashtbl.create (module String) in
    let inventory prefix = function
      | [ L.String account; L.String date ] ->
        let key = account ^ "\000" ^ date in
        let position = 1 + Option.value (Hashtbl.find positions key) ~default:0 in
        Hashtbl.set positions ~key ~data:position;
        [ L.String (Printf.sprintf "%s%s/%d" prefix date position) ]
      | _ -> failwith "inventory ID expects an account and date"
    in
    I.register_globals
      [ "inventory_id", V.caml_func (inventory "")
      ; "observation_id", V.caml_func (inventory "observation/")
      ]
      state;
    let sha256 = Digestif.SHA256.(digest_string contents |> to_hex) in
    let rows = read_rows ~institution ~contents ~separator ~expected ~classify in
    let record_rows, statement_rows =
      List.partition_tf rows ~f:(fun (_, _, _, _, _, kind) -> String.equal kind "record")
    in
    let interpret =
      interpret_row
        ~accounts
        ~balances
        ~account_fn
        ~account
        ~apply
        ~observation
        ~link_statements
        ~sha256
    in
    let records =
      List.map record_rows ~f:(fun row ->
        match interpret row with
        | Record record -> record
        | Statement _ -> assert false)
    in
    let statements =
      if List.is_empty records
      then []
      else
        List.map statement_rows ~f:(fun row ->
          match interpret row with
          | Statement statement -> statement
          | Record _ -> assert false)
    in
    let statement_ids = List.map statements ~f:(fun (s : Records.statement) -> s.id) in
    let records =
      if link_statements && not (List.is_empty statement_ids)
      then
        List.map records ~f:(fun (record : Records.record) ->
          { record with statements = statement_ids })
      else records
    in
    let accounts =
      Hashtbl.to_alist accounts
      |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
      |> List.map ~f:(fun (_, (_, account)) -> account)
    in
    let identity =
      match identity with
      | "source" -> Records.Source
      | "inventory" -> Records.Inventory
      | _ -> fail "unknown identity: %s" identity
    in
    let document : Records.t =
      { institution
      ; identity
      ; converter = Some { name = converter; version = Some version }
      ; sources = [ { name; sha256 } ]
      ; accounts
      ; statements
      ; balances = List.rev !balances
      ; records
      }
    in
    Records.validate_document document |> Or_error.ok_exn)
;;

let source t = t.source

let overlap t : Adapter.overlap =
  if String.equal t.identity "source" then Source_ids else Day_inventory
;;

let tags t = t.tags
let conflict_columns t = t.conflict_columns
let journal_description t = t.journal_description
