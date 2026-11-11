open Base

type reader =
  { profile : Mt940_profile.t
  ; engine : Mt940_transaction.engine
  }

let make profile = { profile; engine = Mt940_transaction.make profile }

let fail reader fmt =
  Printf.ksprintf
    (fun message -> failwith ("" ^ reader.profile.name ^ " MT940: " ^ message))
    fmt
;;

let account_and_currency reader statement =
  match
    String.split (String.strip statement.Mt940_grammar.account) ~on:' '
    |> List.filter ~f:(Fn.non String.is_empty)
  with
  | [ account; currency ] -> account, currency
  | _ -> fail reader "expected an account and a currency in field 25"
;;

let account_id reader statement = fst (account_and_currency reader statement)

let currency reader statement =
  let _, currency = account_and_currency reader statement in
  Validation.currency ("" ^ reader.profile.name ^ " currency") currency |> Or_error.ok_exn
;;

let balance reader statement tag =
  let found =
    match String.prefix tag 2 with
    | "60" -> statement.Mt940_grammar.opening
    | "62" -> statement.closing
    | _ -> failwith ("unexpected MT940 balance field " ^ tag)
  in
  match found with
  | Some balance when String.equal balance.tag tag -> balance
  | _ -> fail reader "missing field %s" tag
;;

let opening_on reader statement = (balance reader statement "60F").date
let closing_on reader statement = (balance reader statement "62F").date

let opening reader statement =
  let balance = balance reader statement "60F" in
  Mt940_grammar.signed balance.direction balance.amount
;;

let closing reader statement =
  let balance = balance reader statement "62F" in
  Mt940_grammar.signed balance.direction balance.amount
;;

(* Field 61 states the entry date without a year. Pick the year that puts it
   closest to the value date, which carries one. *)
let booked_on value_on entry_on =
  let entry_year = Date.year value_on in
  let month, day = entry_on in
  let candidates =
    List.filter_map
      [ entry_year - 1; entry_year; entry_year + 1 ]
      ~f:(fun year ->
        Or_error.try_with (fun () ->
          let date = year, month, day in
          Date.validate_exn date;
          date)
        |> Result.ok)
  in
  match
    List.min_elt candidates ~compare:(fun a b ->
      Int.compare (Int.abs (Date.diff a value_on)) (Int.abs (Date.diff b value_on)))
  with
  | Some booked_on -> booked_on
  | None -> failwith "invalid MT940 entry date"
;;

(* Bunq writes field 86 as a sequence of /TAG/value markers. *)
let markers reader memo =
  let remittance = reader.profile.remittance_marker in
  let name_prefix, description =
    match String.substr_index memo ~pattern:remittance with
    | None -> fail reader "missing remittance marker %s" remittance
    | Some position ->
      ( String.prefix memo position
      , String.drop_prefix memo (position + String.length remittance) )
  in
  let counterparty, name =
    match String.chop_prefix name_prefix ~prefix:reader.profile.name_marker with
    | Some name -> "", name
    | None ->
      (match
         ( String.chop_prefix name_prefix ~prefix:reader.profile.counterparty_marker
         , String.substr_index name_prefix ~pattern:reader.profile.name_marker )
       with
       | Some _, Some i when i >= String.length reader.profile.counterparty_marker ->
         ( String.sub
             name_prefix
             ~pos:(String.length reader.profile.counterparty_marker)
             ~len:(i - String.length reader.profile.counterparty_marker)
         , String.drop_prefix name_prefix (i + String.length reader.profile.name_marker) )
       | _ -> fail reader "unsupported counterparty fields in field 86")
  in
  counterparty, name, description
;;

let entries reader statement =
  let account_id = account_id reader statement
  and currency = currency reader statement in
  let counts = Hashtbl.create (module String) in
  Or_error.try_with (fun () ->
    List.map statement.Mt940_grammar.entries ~f:(fun entry ->
      let value_on = entry.value_on in
      let booked_on =
        match entry.entry_on with
        | Some day -> booked_on value_on day
        | None -> value_on
      in
      if Int.abs (Date.diff booked_on value_on) > 183
      then fail reader "entry date is too far from value date";
      let memo =
        match entry.memo with
        | Some memo -> memo
        | None -> fail reader "missing field 86"
      in
      let bunq_entry =
        Option.is_some entry.entry_on
        && (match entry.direction with
            | Credit | Debit -> true
            | _ -> false)
        && Option.is_none entry.funds_code
        && String.is_prefix entry.transaction_type ~prefix:"N"
        && String.for_all (String.drop_prefix entry.transaction_type 1) ~f:(fun c ->
          Char.(c >= 'A' && c <= 'Z'))
        && String.equal entry.reference "NONREF"
        && Option.is_none entry.bank_reference
        && Option.is_none entry.supplementary
      in
      if not bunq_entry then fail reader "unsupported entry";
      let counterparty, name, description = markers reader memo in
      let amount = Mt940_grammar.signed entry.direction entry.amount in
      let key =
        Mt940_transaction.key
          ~account_id
          ~booked_on
          ~value_on
          ~amount
          ~counterparty
          ~name
          ~description
      in
      Hashtbl.incr counts key;
      Mt940_transaction.create
        reader.engine
        ~account_id
        ~currency
        ~booked_on
        ~value_on
        ~amount
        ~counterparty
        ~name
        ~description
        ~occurrence:(Hashtbl.find_exn counts key)
      |> Or_error.ok_exn))
;;
