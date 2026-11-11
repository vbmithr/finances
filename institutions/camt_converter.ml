open Base

(* A CAMT statement's entries are counted records. Which rows belong together is fin's business,
   apart from what the institution's own references state. *)

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
  ; balance_after = None
  ; valuations = []
  ; names = []
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
  let group =
    match tx.grouping with
    | Camt_source.Profile_group -> Camt_profile.operation_group_key profile tx.reference
    | Interpreted_group group -> group
  in
  let%map occurred = Records.temporal_of_string tx.booked_at_source in
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
  { id = statement.statement_id
  ; account = camt_account_id statement.account
  ; period =
      Some
        ( Records.of_instant statement.period_start
        , Records.of_instant statement.period_end )
  ; total = None
  }
;;

let camt_balances (statement : Camt_source.statement) =
  let balance balance_type quantity at : Records.balance =
    { account = camt_account_id statement.account
    ; balance_type
    ; quantity
    ; commodity = Commodity.fiat statement.account.currency
    ; as_of = Records.of_instant at
    }
  in
  [ balance Opening statement.opening statement.opening_at
  ; balance Closing statement.closing statement.closing_at
  ]
;;

let of_camt ?interpret_entry (profile : Camt_profile.t) contents =
  let open Or_error.Let_syntax in
  let%bind (document : Camt_source.t) =
    Camt_source.parse (Camt_source.create ?interpret_entry profile) contents
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
  ( accounts
  , List.map document.statements ~f:camt_statement
  , List.concat_map document.statements ~f:camt_balances
  , records )
;;

let recognizes contents =
  String.is_substring (String.prefix contents 4096) ~substring:"camt.053"
;;

let convert ?interpret_entry (profile : Camt_profile.t) ~name contents =
  let open Or_error.Let_syntax in
  let%map accounts, statements, balances, records =
    of_camt ?interpret_entry profile contents
  in
  ({ institution = profile.institution
   ; identity = Source
   ; converter = Some { name = "fin camt"; version = Some "1" }
   ; sources = [ { name; sha256 = Digestif.SHA256.(digest_string contents |> to_hex) } ]
   ; accounts
   ; statements
   ; balances
   ; records
   }
   : Records.t)
;;
