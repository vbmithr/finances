open Base

(* Wise states one payment in two files. CAMT is the statement, so its entries are
   the counted records. The CSV has more to say about the same rows, so its entries
   are observations. Which rows belong together is fin's business, not this file's,
   apart from what Wise's own references state. *)

let preset =
  lazy
    (match Builtin.camt_preset "wise" with
     | Some preset -> preset
     | None -> failwith "the bundled Wise definition has no CAMT source")
;;

let movement quantity currency : Records.movement =
  { role = "primary"; quantity; commodity = Commodity.fiat currency }
;;

let no_details : Records.details =
  { descriptions = []
  ; movement_type = None
  ; counterparty = None
  ; references = None
  ; exchange = None
  ; reported = None
  ; fields = []
  }
;;

let references servicer payment : Records.references =
  { servicer = Some servicer; end_to_end = None; payment; hash = None }
;;

(* Keeps the bank's own account ID, so that two profiles' balances in one currency
   stay apart. The CSV cannot name it, see [csv_account]. *)
let camt_account_id (account : Camt_source.account) =
  account.account_id ^ "/" ^ account.account_type
;;

let camt_account (account : Camt_source.account) : Records.account =
  let iban =
    match Iban.of_string account.account_id with
    | Ok _ -> Some account.account_id
    | Error _ -> None
  in
  { id = camt_account_id account
  ; bank = Some account.bank_id
  ; number = (if Option.is_none iban then Some account.account_id else None)
  ; account_type = Some account.account_type
  ; currency = Some account.currency
  ; iban =
      Option.value_map iban ~default:Records.Unstated ~f:(fun iban -> Records.Iban iban)
  }
;;

(* The same parts as the CAMT source event's identity: a reference alone is shared by
   a payment and its refund, and by both sides of a conversion. *)
let camt_record profile statement_id (tx : Camt_source.transaction) =
  let open Or_error.Let_syntax in
  let%map occurred = Records.temporal_of_string tx.booked_at_source in
  let group = Camt_profile.operation_group_key profile tx.reference in
  ({ id =
       String.concat
         ~sep:"/"
         [ tx.reference
         ; Int64.to_string (Timestamp.nanoseconds_since_epoch_exn tx.booked_at)
         ; (if tx.credit then "CRDT" else "DBIT")
         ]
   ; account = camt_account_id tx.account
   ; statements = [ statement_id ]
   ; occurred
   ; value_on = tx.value_on
   ; movements = [ movement tx.amount tx.account.currency ]
   ; fee = None
   ; group = Option.map group ~f:fst
   ; supporting = Option.value_map group ~default:false ~f:snd
   ; observation = false
   ; details =
       { no_details with
         descriptions = [ tx.description ]
       ; references = Some (references tx.reference None)
       ; fields = [ "reference", tx.reference ]
       }
   }
   : Records.record)
;;

let camt_statement (statement : Camt_source.statement) : Records.statement =
  let balance quantity at : Records.balance =
    { quantity; as_of = Records.of_instant at }
  in
  { id = statement.statement_id
  ; account = camt_account_id statement.account
  ; currency = statement.account.currency
  ; period_start = Records.of_instant statement.period_start
  ; period_end = Records.of_instant statement.period_end
  ; opening = balance statement.opening statement.opening_at
  ; closing = balance statement.closing statement.closing_at
  }
;;

let of_camt (preset : Camt_institution_preset.t) contents =
  let open Or_error.Let_syntax in
  let profile = preset.camt_profile in
  let%bind (document : Camt_source.t) =
    Camt_source.parse (Camt_source.create profile) contents
  in
  let%map records =
    List.concat_map document.statements ~f:(fun statement ->
      List.map statement.transactions ~f:(camt_record profile statement.statement_id))
    |> Or_error.combine_errors
  in
  let accounts =
    List.map document.statements ~f:(fun statement -> camt_account statement.account)
    |> List.dedup_and_sort ~compare:(fun (a : Records.account) b ->
      String.compare a.id b.id)
  in
  accounts, List.map document.statements ~f:camt_statement, records
;;

(* The CSV names no account, only a currency in each row, so a CSV with no rows has
   no accounts. An observation pairs by its reference across the institution, so this
   account never holds a counted record. *)
let csv_account currency : Records.account =
  { id = "balance-" ^ Iso4217.to_alpha_code currency
  ; bank = Some "wise"
  ; number = None
  ; account_type = None
  ; currency = Some currency
  ; iban = Unstated
  }
;;

let csv_details ~credit ~reference (fields : (string * string) list) : Records.details =
  let stated name =
    List.Assoc.find fields ~equal:String.equal name
    |> Option.filter ~f:(fun value -> not (String.is_empty value))
  in
  let quantity name =
    Option.bind (stated name) ~f:(fun text ->
      Option.try_with (fun () -> Quantity.of_string text)
      |> Option.filter ~f:Quantity.(fun value -> value >= zero))
  in
  let exchange : Records.exchange option =
    match stated "Exchange From", stated "Exchange To" with
    | Some source, Some target ->
      Some
        { source
        ; target
        ; rate = quantity "Exchange Rate"
        ; target_quantity = quantity "Exchange To Amount"
        }
    | _ -> None
  in
  let counterparty : Records.counterparty option =
    let name = stated (if credit then "Payer Name" else "Payee Name") in
    let iban =
      if credit
      then None
      else
        Option.filter (stated "Payee Account Number") ~f:(fun number ->
          Result.is_ok (Iban.of_string number))
    in
    Option.some_if
      (Option.is_some name || Option.is_some iban)
      ({ name; iban } : Records.counterparty)
  in
  { no_details with
    descriptions = List.filter_opt [ stated "Description"; stated "Merchant" ]
  ; counterparty
  ; references = Some (references reference (stated "Payment Reference"))
  ; exchange
  ; fields = List.filter fields ~f:(fun (_, value) -> not (String.is_empty value))
  }
;;

(* The CSV clock is a local wall clock with no offset, so the row states a date and
   the clock stays in its fields. Total fees repeats a figure that has its own row,
   and stays there too. *)
let csv_record (preset : Camt_institution_preset.t) (row : Csv_clock_companion.row)
  : Records.record
  =
  let profile = preset.csv_profile in
  let fields = List.zip_exn profile.header row.fields in
  let stated column = List.Assoc.find_exn fields ~equal:String.equal column in
  let direction = stated profile.columns.direction in
  { id =
      String.concat ~sep:"/" [ row.reference; stated profile.columns.datetime; direction ]
  ; account = (csv_account row.currency).id
  ; statements = []
  ; occurred = Records.of_date (Ptime.to_date ~tz_offset_s:0 row.booked_at)
  ; value_on = None
  ; movements = [ movement row.amount row.currency ]
  ; fee = None
  ; group = None
  ; supporting = false
  ; observation = true
  ; details =
      csv_details
        ~credit:(String.equal direction "CREDIT")
        ~reference:row.reference
        fields
  }
;;

let of_csv (preset : Camt_institution_preset.t) contents =
  let open Or_error.Let_syntax in
  let%map rows =
    Csv_clock_companion.parse preset.csv_profile preset.camt_profile contents
  in
  let currencies =
    List.map rows ~f:(fun (row : Csv_clock_companion.row) ->
      Iso4217.to_alpha_code row.currency, row.currency)
    |> List.dedup_and_sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  ( List.map currencies ~f:(fun (_, currency) -> csv_account currency)
  , []
  , List.map rows ~f:(csv_record preset) )
;;

let is_camt contents =
  String.is_substring (String.prefix contents 4096) ~substring:"camt.053"
;;

let recognizes contents =
  let preset = Lazy.force preset in
  is_camt contents
  || Csv_clock_companion.recognizes preset.csv_profile preset.camt_profile contents
;;

let convert ~name contents =
  let open Or_error.Let_syntax in
  let preset = Lazy.force preset in
  let%map accounts, statements, records =
    if is_camt contents
    then of_camt preset contents
    else if recognizes contents
    then of_csv preset contents
    else Or_error.error_string "not a Wise CAMT.053 statement or CSV export"
  in
  ({ institution = preset.institution
   ; identity = Source
   ; converter = Some { name = "fin wise"; version = Some "1" }
   ; sources = [ { name; sha256 = Digestif.SHA256.(digest_string contents |> to_hex) } ]
   ; accounts
   ; statements
   ; records
   }
   : Records.t)
;;
