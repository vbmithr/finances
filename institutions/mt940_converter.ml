open Base

(* MT940 states a period with opening and closing balances, and entries with a value date and a
   booking date but no identifier, so identical rows can only be told apart by position. *)

let recognizes = Mt940_source.recognizes

let convert (profile : Mt940_profile.t) ~name contents =
  let open Or_error.Let_syntax in
  let%map (document : Mt940_source.t) =
    Mt940_source.parse (Mt940_transaction.make profile) contents
  in
  let currency = Commodity.fiat document.currency in
  let is_iban = Result.is_ok (Iban.of_string document.account_id) in
  let statement_id =
    String.concat
      ~sep:"/"
      [ document.account_id
      ; Date.to_string document.opening_on
      ; Date.to_string document.closing_on
      ]
  in
  let balance balance_type quantity on : Records.balance =
    { account = document.account_id
    ; balance_type
    ; quantity
    ; commodity = currency
    ; as_of = Records.of_date on
    }
  in
  let positions = Hashtbl.create (module String) in
  let record (tx : Mt940_transaction.t) : Records.record =
    let day = Date.to_string tx.booked_on in
    let position = 1 + Option.value (Hashtbl.find positions day) ~default:0 in
    Hashtbl.set positions ~key:day ~data:position;
    let name = Option.some_if (not (String.is_empty tx.name)) tx.name in
    let iban =
      Option.some_if (Result.is_ok (Iban.of_string tx.counterparty)) tx.counterparty
    in
    { id = Printf.sprintf "%s/%d" day position
    ; account = document.account_id
    ; statements = [ statement_id ]
    ; occurred = Records.of_date tx.booked_on
    ; value_on = Some tx.value_on
    ; movements = [ { role = "primary"; quantity = tx.amount; commodity = currency } ]
    ; fee = None
    ; group = None
    ; supporting = false
    ; observation = false
    ; details =
        { descriptions =
            List.filter [ tx.description; tx.name ] ~f:(Fn.non String.is_empty)
        ; movement_type = None
        ; counterparty =
            Option.some_if
              (Option.is_some name || Option.is_some iban)
              ({ name; iban } : Records.counterparty)
        ; references = None
        ; exchange = None
        ; reported = None
        ; balance_after = None
        ; valuations = []
        ; fields =
            List.filter
              [ "name", tx.name; "counterparty", tx.counterparty ]
              ~f:(fun (_, value) -> not (String.is_empty value))
        }
    }
  in
  ({ institution = profile.institution
   ; identity = Inventory
   ; converter = Some { name = "fin mt940"; version = Some "1" }
   ; sources = [ { name; sha256 = Digestif.SHA256.(digest_string contents |> to_hex) } ]
   ; accounts =
       [ { id = document.account_id
         ; bank = Some profile.institution
         ; number = Option.some_if (not is_iban) document.account_id
         ; account_type =
             Some (profile.account_prefix ^ Iso4217.to_alpha_code document.currency)
         ; currency = Some document.currency
         ; iban = (if is_iban then Iban document.account_id else Unstated)
         }
       ]
   ; statements =
       [ { id = statement_id
         ; account = document.account_id
         ; period =
             Some
               (Records.of_date document.opening_on, Records.of_date document.closing_on)
         ; total = None
         }
       ]
   ; balances =
       [ balance Opening document.opening document.opening_on
       ; balance Closing document.closing document.closing_on
       ]
   ; records = List.map document.transactions ~f:record
   }
   : Records.t)
;;
