open Base

(* The transport is shared; each format chooses its delimiter and whether its
   header is trimmed. Raw data fields are left untouched. *)
let channel ~separator contents =
  let contents =
    Option.value (String.chop_prefix contents ~prefix:"\239\187\191") ~default:contents
  in
  Csv.of_string
    ~separator
    ~strip:false
    ~backslash_escape:false
    ~excel_tricks:false
    ~fix:false
    contents
;;

let header_matches ~trim ~header input =
  let fields = Csv.next input in
  let fields = if trim then List.map fields ~f:String.strip else fields in
  List.equal String.equal fields header
;;

let recognizes ~separator ~trim ~header contents =
  match
    Or_error.try_with (fun () ->
      header_matches ~trim ~header (channel ~separator contents))
  with
  | Ok result -> result
  | Error _ -> false
;;

let check_header ~trim ~header ~message input =
  if not (header_matches ~trim ~header input) then failwith message
;;

let check_utf8 ~name contents =
  if not (Stdlib.String.is_valid_utf_8 contents)
  then failwith (name ^ " CSV must be UTF-8")
;;
