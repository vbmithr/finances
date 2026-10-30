open Base

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : Iso4217.t
  }

type transaction =
  { account : account
  ; reference : string
  ; booked_at : Ptime.t
  ; booked_at_source : string
  ; booked_on : Date.t
  ; value_on : Date.t option
  ; amount : Quantity.t
  ; credit : bool
  ; description : string
  ; source_id : string
  ; payload : string
  }

type statement =
  { account : account
  ; statement_id : string
  ; period_start : Ptime.t
  ; period_end : Ptime.t
  ; opening_at : Ptime.t
  ; closing_at : Ptime.t
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; transactions : transaction list
  }

type t =
  { contents : string
  ; statements : statement list
  }

let account_identity a =
  Json.encode (Jsont.list Jsont.string) [ a.bank_id; a.account_id; a.account_type ]
;;

let account_of_identity (profile : Camt_profile.t) identity currency =
  match Json.decode (Jsont.list Jsont.string) identity |> Or_error.ok_exn with
  | [ bank_id; account_id; account_type ]
    when String.equal bank_id profile.institution
         && String.equal
              account_type
              (profile.account_type_prefix ^ Iso4217.to_alpha_code currency) ->
    { bank_id; account_id; account_type; currency }
  | _ -> failwith "invalid CAMT balance account"
;;

let valid_hex ~length text =
  String.length text = length
  && String.for_all text ~f:(function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false)
;;

let normalize_reference (profile : Camt_profile.t) ~description reference =
  match profile.reference_normalization with
  | Camt_profile.Preserve -> reference
  | Canonical_hex config ->
    (match String.chop_prefix reference ~prefix:config.prefix with
     | Some id ->
       let id = String.filter id ~f:(fun c -> not (Char.equal c '-')) in
       if not (valid_hex ~length:config.length id)
       then failwith "invalid normalized reference";
       config.prefix ^ String.lowercase id
     | None
       when valid_hex ~length:config.length reference
            && String.Caseless.equal (String.strip description) config.description ->
       config.prefix ^ String.lowercase reference
     | None -> reference)
;;

let timestamp = Timestamp.of_rfc3339_exn

(* The CSV names the source-local wall clock without an offset. Interpret that
   clock as UTC for this comparison key, independently of the actual CAMT instant. *)
let matching_time (tx : transaction) =
  let offset =
    match Ptime.of_rfc3339 ~strict:true tx.booked_at_source with
    | Ok (_, offset, _) -> Option.value offset ~default:0
    | Error _ -> invalid_arg "invalid CAMT source booking clock"
  in
  match Ptime.add_span tx.booked_at (Ptime.Span.of_int_s offset) with
  | Some local -> Ptime.truncate ~frac_s:3 local
  | None -> invalid_arg "CAMT local matching clock is outside the Ptime range"
;;

let make_transaction
      (profile : Camt_profile.t)
      codec
      account
      ~reference
      ~booked_at
      ~value_on
      ~amount
      ~credit
      ~description
  =
  if (credit && Quantity.(amount < zero)) || ((not credit) && Quantity.(amount > zero))
  then failwith "CAMT amount sign disagrees with direction";
  let booked_at_source = booked_at in
  let instant = timestamp booked_at in
  let instant_ns = Int64.to_string (Timestamp.nanoseconds_since_epoch_exn instant) in
  let booked_on = Date.of_string (String.prefix booked_at 10) in
  let reference = normalize_reference profile ~description reference in
  ignore (Validation.text "CAMT reference" reference |> Or_error.ok_exn : string);
  let source_id =
    Json.encode
      (Jsont.list Jsont.string)
      [ reference; instant_ns; (if credit then "CRDT" else "DBIT") ]
  in
  let tx =
    { account
    ; reference
    ; booked_at = instant
    ; booked_at_source
    ; booked_on
    ; value_on
    ; amount
    ; credit
    ; description
    ; source_id
    ; payload = ""
    }
  in
  { tx with payload = Json.encode (Lazy.force codec) tx }
;;

let make_codec (profile : Camt_profile.t) =
  let rec codec =
    lazy
      (Jsont.Object.map
         (fun
             ()
              identity
              currency
              reference
              booked_at
              value_on
              amount
              credit
              description
            ->
            Json.validated (fun () ->
              let account = account_of_identity profile identity currency in
              make_transaction
                profile
                codec
                account
                ~reference
                ~booked_at
                ~value_on
                ~amount
                ~credit
                ~description))
       |> Jsont.Object.mem "format" (Json.tag profile.payload_tag) ~enc:(fun _ -> ())
       |> Jsont.Object.mem "account" Jsont.string ~enc:(fun (t : transaction) ->
         account_identity t.account)
       |> Jsont.Object.mem "currency" Json.currency ~enc:(fun (t : transaction) ->
         t.account.currency)
       |> Jsont.Object.mem "reference" Jsont.string ~enc:(fun t -> t.reference)
       |> Jsont.Object.mem "booked_at" Jsont.string ~enc:(fun t -> t.booked_at_source)
       |> Jsont.Object.mem "value_on" (Jsont.option Json.date) ~enc:(fun t -> t.value_on)
       |> Jsont.Object.mem "amount" Json.quantity ~enc:(fun t -> t.amount)
       |> Jsont.Object.mem "credit" Jsont.bool ~enc:(fun t -> t.credit)
       |> Jsont.Object.mem "description" Jsont.string ~enc:(fun t -> t.description)
       |> Jsont.Object.finish)
  in
  codec
;;

type engine =
  { profile : Camt_profile.t
  ; codec : transaction Jsont.t Lazy.t
  }

let create profile = { profile; codec = make_codec profile }
let jsont engine = Lazy.force engine.codec

let direction = function
  | Some Camt.Credit -> true
  | Some Camt.Debit -> false
  | _ -> failwith "invalid CAMT credit/debit indicator"
;;

let required_value name = function
  | Some value -> value
  | None -> Printf.failwithf "missing CAMT %s" name ()
;;

let required_datetime name = function
  | Some (Camt.Date_time value | Both (_, value)) -> value
  | _ -> Printf.failwithf "expected CAMT datetime for %s" name ()
;;

let signed_amount currency amount direction =
  let amount : Camt.amount = required_value "amount" amount in
  let declared = required_value "amount currency" amount.currency in
  if not (String.equal (Iso4217.to_alpha_code currency) declared)
  then failwith "CAMT entry currency differs from balance account currency";
  let amount = Quantity.of_string amount.value in
  if Quantity.(amount < zero)
  then failwith "CAMT amount must be unsigned before applying credit/debit indicator";
  if direction then amount else Quantity.neg amount
;;

let parse_statement engine node =
  let profile = engine.profile in
  let namespace = profile.namespace in
  let view = Camt.statement_view ~namespace node |> Or_error.ok_exn in
  let acct : Camt.account = required_value "account" view.account in
  Option.iter profile.servicer_contains ~f:(fun expected ->
    let servicer = required_value "account servicer" acct.servicer_name in
    if
      not
        (String.is_substring
           (String.lowercase servicer)
           ~substring:(String.lowercase expected))
    then failwith "CAMT account servicer does not match the profile");
  let account_id =
    match acct.id with
    | Some (Camt.Iban value | Other_account_id value) -> value
    | None -> failwith "missing CAMT account ID"
  in
  let currency =
    required_value "account currency" acct.currency
    |> Validation.currency "CAMT account currency"
    |> Or_error.ok_exn
  in
  ignore (Validation.text "CAMT account ID" account_id |> Or_error.ok_exn : string);
  let account =
    { bank_id = profile.institution
    ; account_id
    ; account_type = profile.account_type_prefix ^ Iso4217.to_alpha_code currency
    ; currency
    }
  in
  let statement_id = required_value "statement ID" view.id in
  let period_start = timestamp (required_datetime "period start" view.period_start)
  and period_end = timestamp (required_datetime "period end" view.period_end) in
  if Ptime.compare period_start period_end > 0
  then failwith "reversed CAMT statement interval";
  List.iter view.balances ~f:(fun (balance : Camt.balance) ->
    match balance.code with
    | Some (Camt.Standard_balance _) -> ()
    | _ -> failwith "expected CAMT balance type code");
  let balance code =
    let balances =
      List.filter view.balances ~f:(fun (balance : Camt.balance) ->
        Poly.equal balance.code (Some (Camt.Standard_balance code)))
    in
    match balances with
    | [ balance ] ->
      let at = required_datetime "balance date" balance.as_of in
      signed_amount currency balance.amount (direction balance.direction), timestamp at
    | _ ->
      Printf.failwithf "CAMT statement requires exactly one %s booked balance" code ()
  in
  let opening, opening_at = balance "OPBD"
  and closing, closing_at = balance "CLBD" in
  let transactions =
    List.map view.entries ~f:(fun (entry : Camt.entry) ->
      let nested_status =
        match Camt.children ~namespace "Sts" entry.tree with
        | [ status ] ->
          (match Camt.children ~namespace "Cd" status with
           | [ _ ] -> true
           | _ -> false)
        | _ -> false
      in
      if (not nested_status) || not (Poly.equal entry.status (Some Camt.Booked))
      then failwith "only booked CAMT entries are supported";
      let booked_at = required_datetime "booking date" entry.booking_date in
      let instant = timestamp booked_at in
      if Ptime.compare instant period_start < 0 || Ptime.compare instant period_end > 0
      then failwith "CAMT entry lies outside statement interval";
      let value_on =
        Option.map entry.value_date ~f:(function
          | Camt.Date date | Both (date, _) -> Date.of_string date
          | Date_time date ->
            ignore (timestamp date : Ptime.t);
            Date.of_string (String.prefix date 10))
      in
      let credit = direction entry.direction in
      make_transaction
        profile
        engine.codec
        account
        ~reference:
          (required_value
             "transaction reference"
             (match profile.reference_field with
              | Camt_profile.Proprietary_code -> entry.proprietary_code
              | Account_servicer_reference -> entry.account_servicer_reference))
        ~booked_at
        ~value_on
        ~amount:(signed_amount currency entry.amount credit)
        ~credit
        ~description:(required_value "description" entry.additional_info))
  in
  if
    List.contains_dup
      (List.map transactions ~f:(fun t -> t.source_id))
      ~compare:String.compare
  then
    failwith
      "CAMT statement has indistinguishable entry identities; no entries were collapsed";
  Option.iter view.total_entries ~f:(fun count ->
    if Int.of_string count <> List.length transactions
    then failwith "CAMT statement entry count mismatch");
  let movements =
    List.fold transactions ~init:Quantity.zero ~f:(fun sum tx ->
      Quantity.(sum + tx.amount))
  in
  if not Quantity.(opening + movements = closing)
  then failwith "CAMT booked balances do not reconcile with statement movements";
  { account
  ; statement_id
  ; period_start
  ; period_end
  ; opening_at
  ; closing_at
  ; opening
  ; closing
  ; transactions
  }
;;

let parse engine contents =
  let profile = engine.profile in
  let namespace = profile.namespace in
  let open Or_error.Let_syntax in
  let%bind document = Camt.parse contents in
  Or_error.try_with (fun () ->
    if not (String.equal document.namespace namespace)
    then Printf.failwithf "expected CAMT namespace %s" namespace ();
    let rec check_namespace = function
      | Camt.Text _ -> ()
      | Camt.Element ((ns, _), _, children) ->
        if not (String.equal ns namespace)
        then Printf.failwithf "expected CAMT namespace %s" namespace ();
        List.iter children ~f:check_namespace
    in
    check_namespace document.root;
    let statements = List.map document.statements ~f:(parse_statement engine) in
    { contents; statements })
  |> Or_error.tag ~tag:"CAMT"
;;

let source_transaction engine (event : Source_event.t) =
  let profile = engine.profile in
  Or_error.try_with (fun () ->
    if
      not
        (String.equal event.kind profile.event_kind
         && String.equal event.venue profile.institution)
    then failwith "wrong CAMT source institution or event kind";
    let tx = Json.decode (jsont engine) event.payload |> Or_error.ok_exn in
    if
      (not
         (String.equal event.account (account_identity tx.account)
          && String.equal event.source_id tx.source_id))
      || not
           (match event.occurred with
            | Temporal.Instant instant -> Ptime.equal instant tx.booked_at
            | _ -> false)
    then failwith "CAMT source identity or booking timestamp disagrees with payload";
    tx)
  |> fun result -> Or_error.tag result ~tag:"stored CAMT transaction"
;;
