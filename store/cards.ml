open Base
open Rel
open Handle

type t =
  { institution : string
  ; card_reference : string
  ; known_by : string option
  ; card_source_account : string option
  ; account_id : string option
  ; ledger_account : string option
  ; bank_rows : int
  ; first_seen : Date.t option
  ; last_seen : Date.t option
  ; settled : int
  ; undecided : int
  ; purchases_from : Date.t option
  }

let list db =
  let* rows =
    collect
      db
      (statement
         Sql.card_list
         Row.(
           unit
             (fun
                 institution
                  card_reference
                  known_by
                  card_source_account
                  account_id
                  ledger_account
                  bank_rows
                  first_seen
                  last_seen
                  settled
                  undecided
                  purchases_from
                ->
                ( institution
                , card_reference
                , known_by
                , card_source_account
                , account_id
                , ledger_account
                , bank_rows
                , first_seen
                , last_seen
                , settled
                , undecided
                , purchases_from ))
           * text "institution"
           * text "card_reference"
           * option Rel.Type.text "known_by"
           * option Rel.Type.text "card_source_account"
           * option Rel.Type.text "public_id"
           * option Rel.Type.text "ledger_account"
           * int "bank_rows"
           * option Rel.Type.text "first_seen"
           * option Rel.Type.text "last_seen"
           * int "settled"
           * int "undecided"
           * option Rel.Type.text "purchases_from"))
  in
  let date = Option.map ~f:Date.of_string in
  Or_error.try_with (fun () ->
    List.map
      rows
      ~f:
        (fun
          ( institution
          , card_reference
          , known_by
          , card_source_account
          , account_id
          , ledger_account
          , bank_rows
          , first_seen
          , last_seen
          , settled
          , undecided
          , purchases_from )
        ->
        { institution
        ; card_reference
        ; known_by
        ; card_source_account
        ; account_id
        ; ledger_account
        ; bank_rows
        ; first_seen = date first_seen
        ; last_seen = date last_seen
        ; settled
        ; undecided
        ; purchases_from = date purchases_from
        }))
;;

let resolve db reference =
  let* cards = list db in
  let one = function
    | [ card ] -> Ok (Some card)
    | [] -> Ok None
    | _ ->
      Or_error.errorf
        "several cards match %s; give the card as fin card list prints it"
        reference
  in
  let distinct cards =
    List.dedup_and_sort cards ~compare:(fun a b ->
      [%compare: string * string]
        (a.institution, a.card_reference)
        (b.institution, b.card_reference))
    |> List.map ~f:(fun (c : t) ->
      List.find_exn cards ~f:(fun d ->
        String.equal d.institution c.institution
        && String.equal d.card_reference c.card_reference))
  in
  let* exact =
    one
      (distinct (List.filter cards ~f:(fun c -> String.equal c.card_reference reference)))
  in
  match exact with
  | Some card -> Ok card
  | None ->
    let* ending =
      one
        (distinct
           (List.filter cards ~f:(fun c ->
              String.is_suffix c.card_reference ~suffix:reference)))
    in
    Or_error.of_option
      ending
      ~error:(Error.createf "no card %s; see fin card list" reference)
;;

type assignment =
  { card : t
  ; card_source_account : string
  ; account_id : string
  ; ledger_account : string
  }

let is_card_type kind = List.mem [ "CREDITCRD"; "credit-card" ] kind ~equal:String.equal

(* A card account by its ID, as fin account list prints it. *)
let card_account db account_id =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT institution,source_account,ledger_account,source_account->>2 FROM \
         bank_account_mappings WHERE public_id=?"
        (text
         @-> ret
               Row.(
                 t4
                   (text "institution")
                   (text "source_account")
                   (text "ledger_account")
                   (option Rel.Type.text "account_type"))))
  in
  let* found = collect db (query account_id) in
  match found with
  | [ (institution, source_account, ledger_account, kind) ] ->
    if Option.exists kind ~f:is_card_type
    then Ok (institution, source_account, ledger_account)
    else Or_error.errorf "account %s is not a card account" account_id
  | [] -> Or_error.errorf "unknown account ID %s; see fin account list" account_id
  | _ -> Or_error.errorf "account ID %s is ambiguous" account_id
;;

(* The checks [assign] and a restore share: the card's account may change only from none. *)
let check_assignment (card : t) ~institution ~source_account =
  if not (String.equal card.institution institution)
  then Or_error.error_string "the card and the account are at different institutions"
  else (
    match card.known_by, card.card_source_account with
    | None, _ -> Ok true
    | Some _, Some current when String.equal current source_account -> Ok false
    | Some "statement", _ ->
      Or_error.errorf "%s is another card account's own number" card.card_reference
    | Some _, _ ->
      Or_error.errorf
        "%s is assigned to another account; fin card unassign it first"
        card.card_reference)
;;

let preview_assign db ~card ~account_id =
  let* card = resolve db card in
  let* institution, source_account, ledger_account = card_account db account_id in
  let* changes = check_assignment card ~institution ~source_account in
  Ok
    (Option.some_if
       changes
       { card; card_source_account = source_account; account_id; ledger_account })
;;

let write db ~institution ~card_reference ~previous ~next =
  let set =
    match next with
    | Some account ->
      Rel_sql.Stmt.(
        func
          "INSERT INTO card_assignments(institution,card_reference,card_source_account) \
           VALUES (?,?,?)"
          (text @-> text @-> text @-> unit))
        institution
        card_reference
        account
    | None ->
      Rel_sql.Stmt.(
        func
          "DELETE FROM card_assignments WHERE institution=? AND card_reference=?"
          (text @-> text @-> unit))
        institution
        card_reference
  in
  let audit =
    Rel_sql.Stmt.(
      func
        "INSERT INTO \
         card_assignment_changes(institution,card_reference,previous_card_source_account,card_source_account) \
         VALUES (?,?,?,?)"
        (text @-> text @-> option text @-> option text @-> unit))
  in
  let* () = db_result (Db.exec db.conn set) in
  db_result (Db.exec db.conn (audit institution card_reference previous next))
;;

let assign db (a : assignment) =
  transaction db (fun db ->
    write
      db
      ~institution:a.card.institution
      ~card_reference:a.card.card_reference
      ~previous:None
      ~next:(Some a.card_source_account))
;;

let preview_unassign db ~card =
  let* card = resolve db card in
  match card.known_by, card.card_source_account with
  | Some "assigned", Some account ->
    let recorded =
      Rel_sql.Stmt.(
        func
          "SELECT count(*) FROM card_settlement_decisions d JOIN movement_evidence v ON \
           v.event_id=d.event_id JOIN movements m ON m.event_id=d.event_id WHERE \
           m.venue=? AND v.card_reference=? AND d.card_institution=? AND \
           d.card_source_account=?"
          (text @-> text @-> text @-> text @-> ret integer))
    in
    let* count =
      db_result
        (Db.first
           db.conn
           (recorded card.institution card.card_reference card.institution account))
    in
    if Option.value count ~default:0 > 0
    then
      Or_error.errorf
        "settlements were recorded through %s; undo them first (fin settlement history)"
        card.card_reference
    else Ok card
  | Some "statement", _ ->
    Or_error.errorf "%s is its account's own number" card.card_reference
  | _ -> Or_error.errorf "%s is not assigned" card.card_reference
;;

let unassign db (card : t) =
  transaction db (fun db ->
    write
      db
      ~institution:card.institution
      ~card_reference:card.card_reference
      ~previous:card.card_source_account
      ~next:None)
;;

let assignments db =
  collect
    db
    (statement
       "SELECT institution,card_reference,card_source_account FROM card_assignments \
        ORDER BY institution,card_reference"
       Row.(t3 (text "institution") (text "card_reference") (text "card_source_account")))
;;

let restore_assignment db ~institution ~card_reference ~card_source_account =
  let* cards = list db in
  let card =
    List.find cards ~f:(fun c ->
      String.equal c.institution institution
      && String.equal c.card_reference card_reference)
    |> Option.value
         ~default:
           { institution
           ; card_reference
           ; known_by = None
           ; card_source_account = None
           ; account_id = None
           ; ledger_account = None
           ; bank_rows = 0
           ; first_seen = None
           ; last_seen = None
           ; settled = 0
           ; undecided = 0
           ; purchases_from = None
           }
  in
  let kind =
    Rel_sql.Stmt.(
      func
        "SELECT source_account->>2 FROM bank_account_mappings WHERE institution=? AND \
         source_account=?"
        (text @-> text @-> ret Row.(t1 (option Rel.Type.text "account_type"))))
  in
  let* found = db_result (Db.first db.conn (kind institution card_source_account)) in
  let* () =
    match found with
    | Some (Some kind) when is_card_type kind -> Ok ()
    | Some _ -> Or_error.errorf "card %s is assigned to a non-card account" card_reference
    | None -> Or_error.errorf "card %s is assigned to a missing account" card_reference
  in
  let* changes = check_assignment card ~institution ~source_account:card_source_account in
  if not changes
  then Ok false
  else
    let* () =
      write
        db
        ~institution
        ~card_reference
        ~previous:None
        ~next:(Some card_source_account)
    in
    Ok true
;;
