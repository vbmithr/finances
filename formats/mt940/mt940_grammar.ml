open Base
open Angstrom

type direction =
  | Credit
  | Debit
  | Reversal_credit
  | Reversal_debit

type field =
  { tag : string
  ; text : string
  }

type entry =
  { raw : string
  ; value_on : Date.t
  ; entry_on : (int * int) option
  ; direction : direction
  ; funds_code : char option
  ; amount : Quantity.t
  ; transaction_type : string
  ; reference : string
  ; bank_reference : string option
  ; supplementary : string option
  ; memo : string option
  }

type balance =
  { tag : string
  ; date : Date.t
  ; currency : Iso4217.t
  ; direction : direction
  ; amount : Quantity.t
  }

type statement =
  { reference : string
  ; account : string
  ; number : string
  ; opening : balance option
  ; closing : balance option
  ; entries : entry list
  ; fields : field list
  }

type t = statement list

let digit c = Char.(c >= '0' && c <= '9')
let upper c = Char.(c >= 'A' && c <= 'Z')

let fixed_digits n =
  take n
  >>= fun s -> if String.for_all s ~f:digit then return s else fail "expected digits"
;;

let number s = Int.of_string s

let date =
  fixed_digits 6
  >>| fun s ->
  Date.of_string
    ("20"
     ^ String.prefix s 2
     ^ "-"
     ^ String.sub s ~pos:2 ~len:2
     ^ "-"
     ^ String.drop_prefix s 4)
;;

let entry_date =
  option
    None
    (fixed_digits 4
     >>| fun s -> Some (number (String.prefix s 2), number (String.drop_prefix s 2)))
;;

let direction =
  choice
    [ string "RC" *> return Reversal_credit
    ; string "RD" *> return Reversal_debit
    ; char 'C' *> return Credit
    ; char 'D' *> return Debit
    ]
;;

let signed direction amount =
  match direction with
  | Credit | Reversal_debit -> amount
  | Debit | Reversal_credit -> Quantity.neg amount
;;

let amount =
  take_while1 digit
  >>= fun whole ->
  char ',' *> take_while digit
  >>| fun fraction ->
  Quantity.of_string (if String.is_empty fraction then whole else whole ^ "." ^ fraction)
;;

let entry_parser =
  date
  >>= fun value_on ->
  entry_date
  >>= fun entry_on ->
  direction
  >>= fun direction ->
  option None (satisfy upper >>| Option.some)
  >>= fun funds_code ->
  amount
  >>= fun amount ->
  take 4
  >>= fun transaction_type ->
  if
    not
      (String.length transaction_type = 4
       && List.mem [ 'N'; 'S'; 'F' ] transaction_type.[0] ~equal:Char.equal
       && String.for_all (String.drop_prefix transaction_type 1) ~f:(fun c ->
         upper c || digit c))
  then fail "invalid transaction type"
  else
    take_while (fun _ -> true)
    >>| fun rest ->
    let reference_line, supplementary =
      match String.lsplit2 rest ~on:'\n' with
      | None -> rest, None
      | Some (first, tail) -> first, Option.some_if (not (String.is_empty tail)) tail
    in
    let reference, bank_reference =
      match String.substr_index reference_line ~pattern:"//" with
      | None -> reference_line, None
      | Some i ->
        String.prefix reference_line i, Some (String.drop_prefix reference_line (i + 2))
    in
    { raw = ""
    ; value_on
    ; entry_on
    ; direction
    ; funds_code
    ; amount
    ; transaction_type
    ; reference
    ; bank_reference
    ; supplementary
    ; memo = None
    }
;;

let balance_parser tag =
  direction
  >>= fun direction ->
  date
  >>= fun date ->
  take 3
  >>= fun code ->
  (match Iso4217.of_alpha_code code with
   | Some currency -> return currency
   | None -> fail "invalid currency")
  >>= fun currency -> amount >>| fun amount -> { tag; date; currency; direction; amount }
;;

let parse_field parser tag text =
  match parse_string ~consume:All parser text with
  | Ok value -> value
  | Error e -> failwith ("invalid MT940 field " ^ tag ^ ": " ^ e)
;;

let tag_parser =
  char ':' *> take_while1 (fun c -> upper c || digit c)
  <* char ':'
  >>= fun tag -> take_while (fun _ -> true) >>| fun text -> { tag; text }
;;

let lines_parser =
  let line =
    peek_char
    >>= function
    | None -> fail "end of input"
    | Some _ -> take_till (Char.equal '\n') <* option () (char '\n' *> return ())
  in
  many line <* end_of_input
;;

let statement (fields : field list) =
  let fields = List.rev fields in
  let required name =
    match List.filter fields ~f:(fun (f : field) -> String.equal f.tag name) with
    | [ f ] -> String.strip f.text
    | _ -> failwith ("expected exactly one MT940 field " ^ name)
  in
  let number =
    match
      List.filter fields ~f:(fun (f : field) ->
        String.equal f.tag "28" || String.equal f.tag "28C")
    with
    | [ f ] -> String.strip f.text
    | _ -> failwith "expected exactly one MT940 field 28 or 28C"
  in
  let balance prefix =
    match
      List.filter fields ~f:(fun (f : field) ->
        List.mem [ prefix ^ "F"; prefix ^ "M" ] f.tag ~equal:String.equal)
    with
    | [] -> None
    | [ f ] -> Some (parse_field (balance_parser f.tag) f.tag (String.strip f.text))
    | _ -> failwith ("duplicate MT940 balance " ^ prefix)
  in
  let rec entries acc = function
    | { tag = "61"; text } :: rest ->
      let raw = String.strip text in
      let entry = parse_field entry_parser "61" raw in
      let memo, rest =
        match rest with
        | { tag = "86"; text } :: tail -> Some (String.strip text), tail
        | _ -> None, rest
      in
      entries ({ entry with raw; memo } :: acc) rest
    | _ :: rest -> entries acc rest
    | [] -> List.rev acc
  in
  { reference = required "20"
  ; account = required "25"
  ; number
  ; opening = balance "60"
  ; closing = balance "62"
  ; entries = entries [] fields
  ; fields
  }
;;

let parse ?(allow_unterminated = false) text =
  Or_error.try_with (fun () ->
    if not (Stdlib.String.is_valid_utf_8 text) then failwith "MT940 input is not UTF-8";
    let lines = parse_field lines_parser "lines" text in
    let finish_field current fields =
      match current with
      | None -> fields
      | Some (tag, parts) ->
        { tag; text = String.concat ~sep:"\n" (List.rev parts) } :: fields
    in
    let rec collect messages fields current started envelope awaiting_trailer = function
      | [] ->
        if awaiting_trailer || envelope then failwith "missing SWIFT block trailer";
        let messages =
          if started
          then (
            if not allow_unterminated then failwith "missing MT940 terminator";
            let last = statement (finish_field current fields) in
            if Option.is_none last.closing
            then failwith "unterminated MT940 statement has no closing balance";
            last :: messages)
          else messages
        in
        if List.is_empty messages then failwith "MT940 input has no statement";
        List.rev messages
      | line :: rest ->
        let line = Option.value (String.chop_suffix line ~suffix:"\r") ~default:line in
        if awaiting_trailer
        then
          if String.is_empty (String.strip line)
          then collect messages fields current started envelope true rest
          else if String.equal line "}"
          then collect messages fields current started false false rest
          else failwith "missing SWIFT block trailer"
        else if String.equal line "-" || String.equal line "-}"
        then (
          if not started then failwith "unexpected MT940 terminator";
          if String.equal line "-}" && not envelope
          then failwith "unexpected SWIFT block trailer";
          let fields = finish_field current fields in
          let awaiting = envelope && String.equal line "-" in
          collect (statement fields :: messages) [] None false awaiting awaiting rest)
        else if String.is_prefix line ~prefix:":"
        then (
          let field = parse_field tag_parser "tag" line in
          let fields = finish_field current fields in
          collect
            messages
            fields
            (Some (field.tag, [ field.text ]))
            true
            envelope
            false
            rest)
        else (
          match current with
          | Some (tag, parts) ->
            collect
              messages
              fields
              (Some (tag, line :: parts))
              started
              envelope
              false
              rest
          | None when (not started) && String.is_substring line ~substring:"{4:" ->
            collect messages fields None false true false rest
          | None
            when (not started)
                 && (List.is_empty messages || String.is_empty (String.strip line)) ->
            collect messages fields None false envelope false rest
          | None -> failwith "unexpected content after MT940 terminator")
    in
    collect [] [] None false false false lines)
;;
