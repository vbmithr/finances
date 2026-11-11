open Base

(* The institution is typed at the TOML boundary. Format-specific profiles
   remain values until their existing JSON codecs validate them. *)
type statement =
  { format : string
  ; settings : (string * Toml.Value.t) list
  }

type file =
  { version : int
  ; institution : string
  ; aliases : string list
  ; kind : string
  ; chain_id : int option
  ; network : string option
  ; own_symbols : Toml.Value.t option
  ; stored_formats : string list
  ; statements : statement list
  ; rules : (string * Toml.Value.t) list
  }

let statement_codec =
  let open Toml.Codec in
  Table.(
    obj (fun format settings -> { format; settings })
    |> mem "format" string ~enc:(fun s -> s.format)
    |> keep_unknown ~enc:(fun s -> s.settings) (Mems.assoc Toml.Value.codec)
    |> finish)
;;

let rules_codec =
  let open Toml.Codec in
  Table.(obj Fn.id |> keep_unknown ~enc:Fn.id (Mems.assoc Toml.Value.codec) |> finish)
;;

let file_codec =
  let open Toml.Codec in
  Table.(
    obj
      (fun
          version
           institution
           aliases
           kind
           chain_id
           network
           own_symbols
           stored_formats
           statements
           rules
         ->
         { version
         ; institution
         ; aliases
         ; kind
         ; chain_id
         ; network
         ; own_symbols
         ; stored_formats
         ; statements
         ; rules
         })
    |> mem "version" int ~enc:(fun f -> f.version)
    |> mem "institution" string ~enc:(fun f -> f.institution)
    |> mem "aliases" (list string) ~enc:(fun f -> f.aliases) ~dec_absent:[]
    |> mem "type" string ~enc:(fun f -> f.kind) ~dec_absent:"bank"
    |> opt_mem "chain_id" int ~enc:(fun f -> f.chain_id)
    |> opt_mem "network" string ~enc:(fun f -> f.network)
    |> opt_mem "own_symbols" Toml.Value.codec ~enc:(fun f -> f.own_symbols)
    |> mem "stored_formats" (list string) ~enc:(fun f -> f.stored_formats) ~dec_absent:[]
    |> mem "statements" (array_of_tables statement_codec) ~enc:(fun f -> f.statements)
    |> mem "rules" rules_codec ~enc:(fun f -> f.rules) ~dec_absent:[]
    |> error_unknown
    |> finish)
;;

(* Resolve CSV files before building the existing, self-contained definition. *)
let decode ~load text =
  Or_error.try_with (fun () ->
    let fail fmt = Printf.ksprintf failwith fmt in
    let parsed =
      match Toml.of_string file_codec text with
      | Ok value -> value
      | Error error -> fail "TOML: %s" (Toml.Error.to_string error)
    in
    let table = function
      | Toml.Value.Table (fields, _) ->
        List.map fields ~f:(fun ((key, _), value) -> key, value)
      | _ -> failwith "expected a TOML table"
    in
    let json_string s = Json.encode Jsont.string s in
    let rec json = function
      | Toml.Value.String (s, _) -> json_string s
      | Int (i, _) -> Int64.to_string i
      | Bool (b, _) -> Bool.to_string b
      | Array (values, _) -> "[" ^ String.concat ~sep:"," (List.map values ~f:json) ^ "]"
      | Table (fields, _) -> object_ (List.map fields ~f:(fun ((k, _), v) -> k, v))
      | _ -> failwith "unsupported TOML value in institution definition"
    and object_ fields = object_raw (List.map fields ~f:(fun (k, v) -> k, json v))
    and object_raw fields =
      "{"
      ^ String.concat
          ~sep:","
          (List.map fields ~f:(fun (k, v) -> json_string k ^ ":" ^ v))
      ^ "}"
    in
    let field fields key = List.Assoc.find fields key ~equal:String.equal in
    let require fields key =
      match field fields key with
      | Some value -> value
      | None -> fail "missing %s" key
    in
    let string = function
      | Toml.Value.String (s, _) -> s
      | _ -> failwith "expected a TOML string"
    in
    let check_fields fields allowed =
      List.iter fields ~f:(fun (key, _) ->
        if not (List.mem allowed key ~equal:String.equal)
        then fail "unknown field: %s" key)
    in
    let csv = ref []
    and lua_csv = ref []
    and statement = ref None
    and camt_script = ref None in
    List.iter parsed.statements ~f:(fun { format; settings } ->
      match format with
      | "csv" ->
        check_fields settings [ "definition"; "script" ];
        (match field settings "definition", field settings "script" with
         | Some value, None ->
           let path = string value in
           let contents = load path |> Or_error.ok_exn in
           let layout = Csv_converter.of_string contents |> Or_error.ok_exn in
           if not (String.equal (Csv_converter.institution layout) parsed.institution)
           then fail "CSV definition %s belongs to a different institution" path;
           csv := !csv @ [ contents ]
         | None, Some value ->
           let path = string value in
           let contents = load path |> Or_error.ok_exn in
           ignore
             (Lua_csv_converter.create ~institution:parsed.institution contents
              |> Or_error.ok_exn
              : Lua_csv_converter.t);
           lua_csv := !lua_csv @ [ json_string contents ]
         | _ -> failwith "a CSV statement needs exactly one definition or script")
      | "ofx" | "camt" | "mt940" ->
        if Option.is_some !statement
        then failwith "only one non-CSV statement format is supported";
        if String.equal format "ofx"
        then check_fields settings []
        else if String.equal format "camt"
        then check_fields settings [ "profile"; "script" ]
        else check_fields settings [ "profile" ];
        if String.equal format "camt"
        then
          Option.iter (field settings "script") ~f:(fun value ->
            let contents = load (string value) |> Or_error.ok_exn in
            ignore (Lua_camt_interpreter.create contents |> Or_error.ok_exn : _ -> _ -> _);
            camt_script := Some contents);
        statement
        := Some
             ( format
             , if String.equal format "ofx"
               then "true"
               else (
                 let profile = table (require settings "profile") in
                 let profile =
                   match field profile "institution" with
                   | None ->
                     ("institution", Toml.Value.string parsed.institution) :: profile
                   | Some value when String.equal (string value) parsed.institution ->
                     profile
                   | Some _ -> failwith "statement profile institution does not match"
                 in
                 let profile =
                   if Option.is_some (field profile "version")
                   then profile
                   else ("version", Toml.Value.int_of_int 1) :: profile
                 in
                 (* Legacy source-event codecs still need their identifiers;
                    the bundled defaults follow the institution name. *)
                 let add_default profile name value =
                   if Option.is_some (field profile name)
                   then profile
                   else (name, Toml.Value.string value) :: profile
                 in
                 let kind = if String.equal format "camt" then "event_kind" else "kind" in
                 let payload_suffix =
                   if String.equal format "camt" then "-camt-v1" else "-v1"
                 in
                 let profile =
                   add_default profile kind (parsed.institution ^ "-transaction")
                 in
                 let profile =
                   add_default profile "payload_tag" (parsed.institution ^ payload_suffix)
                 in
                 if String.equal format "camt" && Option.is_some !camt_script
                 then (
                   List.iter
                     [ "reference_field"; "reference_normalization"; "operation_group" ]
                     ~f:(fun name ->
                       if Option.is_some (field profile name)
                       then fail "CAMT profile %s cannot be used with a script" name);
                   (* The legacy profile codec requires these fields, but Lua owns
                      interpretation and the values are never used for conversion. *)
                   object_raw
                     (("reference_field", json_string "proprietary-code")
                      :: ("reference_normalization", "null")
                      :: List.map profile ~f:(fun (key, value) -> key, json value)))
                 else object_ profile) )
      | _ -> fail "unknown statement format: %s" format);
    let rules =
      List.map parsed.rules ~f:(fun (name, value) ->
        let fields = table value in
        if Option.is_some (field fields "name")
        then failwith "rule name belongs in its table header";
        object_ (("name", Toml.Value.string name) :: fields))
    in
    let record_fields =
      [ "institution", json_string parsed.institution
      ; ( "aliases"
        , "[" ^ String.concat ~sep:"," (List.map parsed.aliases ~f:json_string) ^ "]" )
      ; ( "stored_formats"
        , "["
          ^ String.concat ~sep:"," (List.map parsed.stored_formats ~f:json_string)
          ^ "]" )
      ]
      @ Option.to_list
          (Option.map parsed.chain_id ~f:(fun id -> "chain_id", Int.to_string id))
      @ Option.to_list
          (Option.map parsed.network ~f:(fun name -> "network", json_string name))
      @ Option.to_list
          (Option.map parsed.own_symbols ~f:(fun value -> "own_symbols", json value))
      @ Option.to_list !statement
      @ Option.to_list
          (Option.map !camt_script ~f:(fun source -> "camt_script", json_string source))
      @ (if List.is_empty !csv
         then []
         else [ "csv", "[" ^ String.concat ~sep:"," !csv ^ "]" ])
      @
      if List.is_empty !lua_csv
      then []
      else [ "lua_csv", "[" ^ String.concat ~sep:"," !lua_csv ^ "]" ]
    in
    let obj fields =
      "{"
      ^ String.concat
          ~sep:","
          (List.map fields ~f:(fun (k, v) -> json_string k ^ ":" ^ v))
      ^ "}"
    in
    let full =
      obj
        [ "version", Int.to_string parsed.version
        ; "kind", json_string "records"
        ; "type", json_string parsed.kind
        ; "records", obj record_fields
        ; "rules", "[" ^ String.concat ~sep:"," rules ^ "]"
        ]
    in
    Json.decode Institution_definition.codec full |> Or_error.ok_exn)
;;
