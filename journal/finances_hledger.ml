open Core
open Finances

let header_text text =
  text
  |> String.map ~f:(fun c -> if Char.to_int c < 32 || Char.equal c '\127' then ' ' else c)
  |> fun s -> String.substr_replace_all s ~pattern:";" ~with_:"；"
;;

let transaction (event_id, key, (entry : Accounting.entry)) =
  let postings =
    List.map entry.postings ~f:(fun posting ->
      Hledger.Journal.posting
        posting.account
        ~amount:(Hledger.Amount.create ~commodity:posting.commodity posting.quantity))
  in
  let comment : Hledger.Journal.comment =
    { same_line = Some (sprintf "source-event-id: %Ld" event_id)
    ; lines = [ "projection-key: " ^ key ]
    }
  in
  Hledger.Journal.transaction
    entry.date
    ~code:(sprintf "event-%Ld" event_id)
    ~description:(header_text entry.description)
    ~comment
    postings
;;

let export db =
  let open Or_error.Let_syntax in
  let%map entries = Store.entries db in
  let postings = List.concat_map entries ~f:(fun (_, _, entry) -> entry.postings) in
  let accounts =
    List.map postings ~f:(fun p -> p.account)
    |> List.dedup_and_sort ~compare:String.compare
  in
  let precision = String.Table.create () in
  List.iter postings ~f:(fun p ->
    Hashtbl.update precision p.commodity ~f:(fun previous ->
      Int.max
        (Option.value previous ~default:2)
        (Int.max 0 (-Bigdecimal.exponent p.quantity))));
  let declarations =
    [ Hledger.Journal.Comment_line
        "; Generated from SQLite. Regenerate after database changes."
    ; Comment_line
        "; Bank entries describe movements; opening balances and classifications are not \
         inferred."
    ; Directive (Decimal_mark { mark = '.'; comment = Hledger.Journal.no_comment })
    ]
    @ (Hashtbl.to_alist precision
       |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
       |> List.map ~f:(fun (symbol, precision) ->
         Hledger.Journal.Directive
           (Commodity
              { symbol
              ; sample =
                  Some (Hledger.Amount.create ~commodity:symbol ~precision Bigdecimal.one)
              ; comment = Hledger.Journal.no_comment
              ; subdirectives = []
              })))
    @ List.map accounts ~f:(fun name ->
      Hledger.Journal.Directive
        (Account { name; comment = Hledger.Journal.no_comment; subdirectives = [] }))
  in
  let items =
    declarations
    @ [ Hledger.Journal.Blank ]
    @ List.concat_map entries ~f:(fun entry ->
      [ Hledger.Journal.Transaction (transaction entry); Blank ])
  in
  Hledger.Print.journal items
;;
