open Base

let value_exn = function
  | Ok value -> value
  | Error message -> failwith message
;;

let or_error = function
  | Ok value -> value
  | Error error -> Jsont.Error.msgf Jsont.Meta.none "%s" (Error.to_string_hum error)
;;

let validated ?(meta = Jsont.Meta.none) f =
  try f () with
  | Jsont.Error _ as exn -> raise exn
  | exn -> Jsont.Error.msgf meta "%s" (Exn.to_string exn)
;;

let string_codec ~dec ~enc =
  Jsont.Base.string
    (Jsont.Base.map ~dec:(fun meta text -> validated ~meta (fun () -> dec text)) ~enc ())
;;

(* Jsont accepts repeated object members by default. Validate uniqueness without
   allocating an intermediate JSON tree before decoding snapshots and decisions. *)
let rec unique_members =
  lazy
    (let nested = Jsont.rec' unique_members in
     let arrays =
       Jsont.Array.map
         nested
         ~dec_empty:(fun () -> ())
         ~dec_add:(fun _ () () -> ())
         ~dec_finish:(fun _ _ () -> ())
       |> Jsont.Array.array
     in
     let members =
       Jsont.Object.Mems.map
         nested
         ~dec_empty:(fun () -> Hash_set.create (module String))
         ~dec_add:(fun meta name () seen ->
           if Hash_set.mem seen name
           then Jsont.Error.msgf meta "duplicate JSON member %S" name;
           Hash_set.add seen name;
           seen)
         ~dec_finish:(fun _ _ -> ())
     in
     let objects =
       Jsont.Object.map Fn.id |> Jsont.Object.keep_unknown members |> Jsont.Object.finish
     in
     Jsont.any
       ~dec_null:Jsont.ignore
       ~dec_bool:Jsont.ignore
       ~dec_number:Jsont.ignore
       ~dec_string:Jsont.ignore
       ~dec_array:arrays
       ~dec_object:objects
       ())
;;

let decode codec text =
  Or_error.try_with (fun () ->
    Jsont_bytesrw.decode_string (Lazy.force unique_members) text |> value_exn;
    Jsont_bytesrw.decode_string codec text |> value_exn)
;;

let encode ?(format = Jsont.Minify) codec value =
  (* Small identities dominate encoding calls. Keep scratch local for nested
     encodes; larger values are flushed in chunks rather than limited in size. *)
  Jsont_bytesrw.encode_string ~buf:(Bytes.create 256) ~format codec value |> value_exn
;;

(* Never truncate fractional numbers or round an ID through a float. Large int64
   values use Jsont's exact decimal-string representation. *)
let safe_integer value =
  if
    (not (Float.is_finite value))
    || (not (Float.equal value (Float.round_down value)))
    || Float.(abs value > 9007199254740991.)
  then
    Jsont.Error.msgf
      Jsont.Meta.none
      "expected an exact integer; use a decimal string for large IDs";
  value
;;

let integer =
  let number =
    Jsont.map Jsont.number ~dec:(fun n -> Int.of_float (safe_integer n)) ~enc:Float.of_int
  in
  Jsont.any ~dec_number:number ~dec_string:Jsont.int ~enc:(fun _ -> Jsont.int) ()
;;

let int64_codec =
  let number =
    Jsont.map
      Jsont.number
      ~dec:(fun n -> Int64.of_float (safe_integer n))
      ~enc:Int64.to_float
  in
  Jsont.any ~dec_number:number ~dec_string:Jsont.int64 ~enc:(fun _ -> Jsont.int64) ()
;;

let version expected =
  Jsont.map
    Jsont.number
    ~dec:(fun actual ->
      if not (Float.equal (safe_integer actual) (Float.of_int expected))
      then Jsont.Error.msgf Jsont.Meta.none "unsupported JSON version")
    ~enc:(fun () -> Float.of_int expected)
;;

let date = string_codec ~dec:Date.of_string ~enc:Date.to_string
let quantity = string_codec ~dec:Quantity.of_string ~enc:Quantity.to_string

let currency =
  string_codec
    ~dec:(fun value -> Validation.currency "currency" value |> or_error)
    ~enc:Iso4217.to_alpha_code
;;

let iban =
  string_codec ~dec:(fun text -> Iban.of_string text |> or_error) ~enc:Iban.to_string
;;

let tag expected =
  Jsont.map
    Jsont.string
    ~dec:(fun actual ->
      if not (String.equal actual expected)
      then Jsont.Error.msgf Jsont.Meta.none "unsupported JSON format")
    ~enc:(fun () -> expected)
;;

let members codec =
  Jsont.Object.Mems.map
    codec
    ~dec_empty:(fun () -> [])
    ~dec_add:(fun _ name value fields -> (name, value) :: fields)
    ~dec_finish:(fun _ fields -> List.rev fields)
    ~enc:
      { enc =
          (fun f fields acc ->
            List.fold fields ~init:acc ~f:(fun acc (name, value) ->
              f Jsont.Meta.none name value acc))
      }
;;

let string_map =
  Jsont.Object.map Fn.id
  |> Jsont.Object.keep_unknown (members Jsont.string) ~enc:Fn.id
  |> Jsont.Object.finish
;;
