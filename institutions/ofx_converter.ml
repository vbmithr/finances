open Base

(* OFX states one period and one or two balances per account. Each entry has a FITID, so these
   are counted records with source identity. DTUSER stays out of the record, in the archived file
   only: BoursoBank's is not a valid date, and it changes between exports of one row. *)

let recognizes contents =
  let prefix = String.strip (String.prefix contents 4096) in
  String.is_substring prefix ~substring:"<OFX"
  || String.is_prefix prefix ~prefix:"OFXHEADER:"
;;

let account (account : Ofx_source.account) : Records.account =
  let is_iban = Result.is_ok (Iban.of_string account.account_id) in
  { id = account.account_id
  ; bank = Some account.bank_id
  ; number = Option.some_if (not is_iban) account.account_id
  ; account_type = Some account.account_type
  ; currency = Some account.currency
  ; iban =
      (* OFX credit-card accounts are card numbers, never IBANs. *)
      (if String.equal account.account_type "CREDITCRD"
       then No_iban
       else if is_iban
       then Iban account.account_id
       else Unstated)
  }
;;

let statement_id (statement : Ofx_source.statement) =
  Option.map statement.period ~f:(fun (first, last) ->
    String.concat
      ~sep:"/"
      [ statement.account.account_id; Date.to_string first; Date.to_string last ])
;;

let statement_line (statement : Ofx_source.statement) : Records.statement option =
  Option.map2
    (statement_id statement)
    statement.period
    ~f:(fun id (first, last) : Records.statement ->
      { id
      ; account = statement.account.account_id
      ; period = Some (Records.of_date first, Records.of_date last)
      ; total = None
      })
;;

let balances (statement : Ofx_source.statement) =
  let balance balance_type (b : Ofx_source.balance) : Records.balance =
    { account = statement.account.account_id
    ; balance_type
    ; quantity = b.amount
    ; commodity = Commodity.fiat statement.account.currency
    ; as_of = Records.of_instant b.as_of
    }
  in
  List.filter_opt
    [ Option.map statement.ledger_balance ~f:(balance Closing)
    ; Option.map statement.available_balance ~f:(balance Available)
    ]
;;

let record (statement : Ofx_source.statement) (tx : Ofx_source.transaction)
  : Records.record
  =
  { id = tx.fitid
  ; account = statement.account.account_id
  ; statements = Option.to_list (statement_id statement)
  ; occurred = Records.of_date tx.booked_on
  ; value_on = None
  ; movements =
      [ { role = "primary"
        ; quantity = tx.amount
        ; commodity = Commodity.fiat statement.account.currency
        }
      ]
  ; fee = None
  ; group = None
  ; supporting = false
  ; observation = false
  ; details =
      { descriptions = List.filter_opt [ tx.name; tx.memo ]
      ; movement_type = None
      ; counterparty = None
      ; references =
          Some
            { servicer = Some tx.fitid; end_to_end = None; payment = None; hash = None }
      ; exchange = None
      ; reported = None
      ; balance_after = None
      ; valuations = []
      ; fields =
          List.filter_opt
            [ Option.map tx.name ~f:(fun value -> "name", value)
            ; Option.map tx.memo ~f:(fun value -> "memo", value)
            ; Some ("TRNTYPE", tx.transaction_type)
            ]
      }
  }
;;

let convert ~institution ~name contents =
  let open Or_error.Let_syntax in
  let%map (document : Ofx_source.t) = Ofx_source.parse contents in
  let statements = document.statements in
  ({ institution
   ; identity = Source
   ; converter = Some { name = "fin ofx"; version = Some "1" }
   ; sources = [ { name; sha256 = Digestif.SHA256.(digest_string contents |> to_hex) } ]
   ; accounts = List.map statements ~f:(fun s -> account s.account)
   ; statements = List.filter_map statements ~f:statement_line
   ; balances = List.concat_map statements ~f:balances
   ; records =
       List.concat_map statements ~f:(fun statement ->
         List.map statement.transactions ~f:(record statement))
   }
   : Records.t)
;;
