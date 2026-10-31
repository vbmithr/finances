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

(* Count delimiters outside quotes in the first logical record. Quoted newlines
   and doubled quotes belong to a cell, not the header boundary. *)
let detect_separator contents =
  let length = String.length contents in
  let rec scan i quoted commas semicolons =
    if i >= length
    then if quoted then None else Some (commas, semicolons)
    else (
      match contents.[i] with
      | '"' when quoted && i + 1 < length && Char.equal contents.[i + 1] '"' ->
        scan (i + 2) quoted commas semicolons
      | '"' -> scan (i + 1) (not quoted) commas semicolons
      | ('\n' | '\r') when not quoted -> Some (commas, semicolons)
      | ',' when not quoted -> scan (i + 1) quoted (commas + 1) semicolons
      | ';' when not quoted -> scan (i + 1) quoted commas (semicolons + 1)
      | _ -> scan (i + 1) quoted commas semicolons)
  in
  match scan 0 false 0 0 with
  | Some (commas, 0) when commas > 0 -> Some ','
  | Some (0, semicolons) when semicolons > 0 -> Some ';'
  | _ -> None
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
