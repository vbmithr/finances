open Base
module Bytes_stream = Bytesrw.Bytes

type identity =
  | Source
  | Inventory

type converter =
  { name : string
  ; version : string option
  }

type source =
  { name : string
  ; sha256 : string
  }

type account_iban =
  | Unstated
  | Iban of string
  | No_iban

type account =
  { id : string
  ; bank : string option
  ; number : string option
  ; account_type : string option
  ; currency : Iso4217.t option
  ; iban : account_iban
  }

type temporal =
  { value : Temporal.t
  ; stated : string
  }

let of_date date = { value = Temporal.Date date; stated = Date.to_string date }

let of_instant instant =
  { value = Temporal.Instant instant; stated = Timestamp.to_rfc3339 instant }
;;

let local_date t = Date.of_string (String.prefix t.stated 10)

type amount =
  { quantity : Quantity.t
  ; commodity : Commodity.t
  }

type balance_type =
  | Opening
  | Closing
  | Available
  | Day_end

let balance_type_to_string = function
  | Opening -> "opening"
  | Closing -> "closing"
  | Available -> "available"
  | Day_end -> "day_end"
;;

type balance =
  { account : string
  ; balance_type : balance_type
  ; quantity : Quantity.t
  ; commodity : Commodity.t
  ; as_of : temporal
  }

type statement =
  { id : string
  ; account : string
  ; period : (temporal * temporal) option
  ; total : amount option
  }

type movement =
  { role : string
  ; quantity : Quantity.t
  ; commodity : Commodity.t
  }

type counterparty =
  { name : string option
  ; iban : string option
  }

type references =
  { servicer : string option
  ; end_to_end : string option
  ; payment : string option
  ; hash : string option
  }

type exchange =
  { source : string
  ; target : string
  ; rate : Quantity.t option
  ; target_quantity : Quantity.t option
  }

type reported =
  { quantity : Quantity.t
  ; symbol : string
  }

type details =
  { descriptions : string list
  ; movement_type : Movement_type.t option
  ; counterparty : counterparty option
  ; references : references option
  ; exchange : exchange option
  ; reported : reported option
  ; balance_after : Quantity.t option
  ; valuations : amount list
  ; names : (string * string) list
  ; fields : (string * string) list
  }

type record =
  { id : string
  ; account : string
  ; statements : string list
  ; occurred : temporal
  ; value_on : Date.t option
  ; movements : movement list
  ; fee : Quantity.t option
  ; group : string option
  ; supporting : bool
  ; observation : bool
  ; details : details
  }

type header =
  { institution : string
  ; identity : identity
  ; converter : converter option
  }

type t =
  { institution : string
  ; identity : identity
  ; converter : converter option
  ; sources : source list
  ; accounts : account list
  ; statements : statement list
  ; balances : balance list
  ; records : record list
  }

let refuse fmt = Jsont.Error.msgf Jsont.Meta.none fmt

(* Strings whose shape the schema pins with a pattern. The domain parsers accept
   more, such as exponents in a quantity, which the format does not allow. *)
let shaped label pattern ~dec ~enc =
  let re = Re.compile (Re.whole_string (Re.Perl.re pattern)) in
  Jsont.map
    Jsont.string
    ~dec:(fun text ->
      if not (Re.execp re text) then refuse "invalid %s: %S" label text;
      (* A pattern admits values such as 2025-02-31, which only the parser refuses. *)
      try dec text with
      | Jsont.Error _ as error -> raise error
      | _ -> refuse "invalid %s: %S" label text)
    ~enc
;;

let text =
  Jsont.map
    Jsont.string
    ~dec:(fun text ->
      if String.is_empty text then refuse "expected a nonempty string";
      text)
    ~enc:Fn.id
;;

let token_pattern = "[a-z0-9][a-z0-9_-]*"
let token = shaped "token" token_pattern ~dec:Fn.id ~enc:Fn.id
let date_pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}"
let date = shaped "date" date_pattern ~dec:Date.of_string ~enc:Date.to_string

let temporal_pattern =
  date_pattern ^ "(T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2}))?"
;;

let temporal =
  shaped
    "date or instant"
    temporal_pattern
    ~dec:(fun stated -> { value = Temporal.of_string_exn stated; stated })
    ~enc:(fun t -> t.stated)
;;

let temporal_of_string text = Json.decode temporal (Json.encode Jsont.string text)

let quantity =
  shaped "quantity" "-?[0-9]+(\\.[0-9]+)?" ~dec:Quantity.of_string ~enc:Quantity.to_string
;;

let positive_quantity =
  shaped
    "non-negative quantity"
    "[0-9]+(\\.[0-9]+)?"
    ~dec:Quantity.of_string
    ~enc:Quantity.to_string
;;

let sha256 = shaped "SHA-256" "[0-9a-f]{64}" ~dec:Fn.id ~enc:Fn.id

let currency =
  shaped
    "currency"
    "[A-Z]{3}"
    ~dec:(fun code -> Validation.currency "currency" code |> Json.or_error)
    ~enc:Iso4217.to_alpha_code
;;

let commodity =
  let lower_case = Re.compile (Re.whole_string (Re.Perl.re token_pattern)) in
  Jsont.map
    Commodity.codec
    ~dec:(fun commodity ->
      Option.iter (Commodity.provider commodity) ~f:(fun provider ->
        if not (Re.execp lower_case provider) then refuse "invalid token: %S" provider);
      commodity)
    ~enc:Fn.id
;;

(* A member that is either absent or true; false is not a value of the format. *)
let flag name ~enc map =
  let codec =
    Jsont.map
      Jsont.bool
      ~dec:(fun value -> if not value then refuse "%s can only be true" name)
      ~enc:(fun () -> true)
  in
  Jsont.Object.opt_mem name codec map ~enc:(fun value ->
    if enc value then Some () else None)
;;

let optional_list name codec ~enc map =
  Jsont.Object.mem name (Jsont.list codec) map ~dec_absent:[] ~enc_omit:List.is_empty ~enc
;;

let identity_codec = Jsont.enum [ "source", Source; "inventory", Inventory ]

let converter_codec =
  Jsont.Object.map (fun name version : converter -> { name; version })
  |> Jsont.Object.mem "name" text ~enc:(fun (c : converter) -> c.name)
  |> Jsont.Object.opt_mem "version" text ~enc:(fun (c : converter) -> c.version)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let source_codec =
  Jsont.Object.map (fun name sha256 : source -> { name; sha256 })
  |> Jsont.Object.mem "name" text ~enc:(fun (s : source) -> s.name)
  |> Jsont.Object.mem "sha256" sha256 ~enc:(fun (s : source) -> s.sha256)
  |> Jsont.Object.finish
;;

let account_codec =
  Jsont.Object.map (fun id bank number account_type currency iban no_iban : account ->
    let iban =
      match iban, no_iban with
      | Some _, Some () -> refuse "an account cannot state both iban and no_iban"
      | Some iban, None -> Iban iban
      | None, Some () -> No_iban
      | None, None -> Unstated
    in
    { id; bank; number; account_type; currency; iban })
  |> Jsont.Object.mem "id" text ~enc:(fun (a : account) -> a.id)
  |> Jsont.Object.opt_mem "bank" text ~enc:(fun (a : account) -> a.bank)
  |> Jsont.Object.opt_mem "number" text ~enc:(fun (a : account) -> a.number)
  |> Jsont.Object.opt_mem "type" text ~enc:(fun (a : account) -> a.account_type)
  |> Jsont.Object.opt_mem "currency" currency ~enc:(fun (a : account) -> a.currency)
  |> Jsont.Object.opt_mem "iban" text ~enc:(fun (a : account) ->
    match a.iban with
    | Iban iban -> Some iban
    | Unstated | No_iban -> None)
  |> flag "no_iban" ~enc:(fun (a : account) ->
    match a.iban with
    | No_iban -> true
    | Unstated | Iban _ -> false)
  |> Jsont.Object.finish
;;

let amount_codec =
  Jsont.Object.map (fun quantity commodity : amount -> { quantity; commodity })
  |> Jsont.Object.mem "quantity" quantity ~enc:(fun (a : amount) -> a.quantity)
  |> Jsont.Object.mem "commodity" commodity ~enc:(fun (a : amount) -> a.commodity)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let balance_type_codec =
  Jsont.enum
    [ "opening", Opening; "closing", Closing; "available", Available; "day_end", Day_end ]
;;

let balance_codec =
  Jsont.Object.map (fun account balance_type quantity commodity as_of : balance ->
    { account; balance_type; quantity; commodity; as_of })
  |> Jsont.Object.mem "account" text ~enc:(fun (b : balance) -> b.account)
  |> Jsont.Object.mem "type" balance_type_codec ~enc:(fun (b : balance) -> b.balance_type)
  |> Jsont.Object.mem "quantity" quantity ~enc:(fun (b : balance) -> b.quantity)
  |> Jsont.Object.mem "commodity" commodity ~enc:(fun (b : balance) -> b.commodity)
  |> Jsont.Object.mem "as_of" temporal ~enc:(fun (b : balance) -> b.as_of)
  |> Jsont.Object.finish
;;

let statement_codec =
  Jsont.Object.map (fun id account period_start period_end total : statement ->
    let period =
      match period_start, period_end with
      | Some first, Some last -> Some (first, last)
      | None, None -> None
      | _ -> refuse "a period needs both period_start and period_end"
    in
    { id; account; period; total })
  |> Jsont.Object.mem "id" text ~enc:(fun (s : statement) -> s.id)
  |> Jsont.Object.mem "account" text ~enc:(fun (s : statement) -> s.account)
  |> Jsont.Object.opt_mem "period_start" temporal ~enc:(fun (s : statement) ->
    Option.map s.period ~f:fst)
  |> Jsont.Object.opt_mem "period_end" temporal ~enc:(fun (s : statement) ->
    Option.map s.period ~f:snd)
  |> Jsont.Object.opt_mem "total" amount_codec ~enc:(fun (s : statement) -> s.total)
  |> Jsont.Object.finish
;;

let movement_codec =
  Jsont.Object.map (fun role quantity commodity : movement ->
    { role; quantity; commodity })
  |> Jsont.Object.mem "role" token ~enc:(fun (m : movement) -> m.role)
  |> Jsont.Object.mem "quantity" quantity ~enc:(fun (m : movement) -> m.quantity)
  |> Jsont.Object.mem "commodity" commodity ~enc:(fun (m : movement) -> m.commodity)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let counterparty_codec =
  Jsont.Object.map (fun name iban : counterparty -> { name; iban })
  |> Jsont.Object.opt_mem "name" text ~enc:(fun (c : counterparty) -> c.name)
  |> Jsont.Object.opt_mem "iban" text ~enc:(fun (c : counterparty) -> c.iban)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let references_codec =
  Jsont.Object.map (fun servicer end_to_end payment hash ->
    { servicer; end_to_end; payment; hash })
  |> Jsont.Object.opt_mem "servicer" text ~enc:(fun r -> r.servicer)
  |> Jsont.Object.opt_mem "end_to_end" text ~enc:(fun r -> r.end_to_end)
  |> Jsont.Object.opt_mem "payment" text ~enc:(fun r -> r.payment)
  |> Jsont.Object.opt_mem "hash" text ~enc:(fun r -> r.hash)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let exchange_codec =
  Jsont.Object.map (fun source target rate target_quantity ->
    { source; target; rate; target_quantity })
  |> Jsont.Object.mem "source" text ~enc:(fun e -> e.source)
  |> Jsont.Object.mem "target" text ~enc:(fun e -> e.target)
  |> Jsont.Object.opt_mem "rate" positive_quantity ~enc:(fun e -> e.rate)
  |> Jsont.Object.opt_mem "target_quantity" quantity ~enc:(fun e -> e.target_quantity)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let reported_codec =
  Jsont.Object.map (fun quantity symbol : reported -> { quantity; symbol })
  |> Jsont.Object.mem "quantity" quantity ~enc:(fun (r : reported) -> r.quantity)
  |> Jsont.Object.mem "symbol" text ~enc:(fun (r : reported) -> r.symbol)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let movement_type =
  Jsont.map text ~dec:Movement_type.of_string ~enc:Movement_type.to_string
;;

(* A name may repeat, so fields are pairs in source order rather than an object. *)
let field =
  Jsont.map
    (Jsont.list Jsont.string)
    ~dec:(function
      | [ name; value ] when not (String.is_empty name) -> name, value
      | _ -> refuse "a field must be a nonempty name and a value")
    ~enc:(fun (name, value) -> [ name; value ])
;;

let details_codec =
  Jsont.Object.map
    (fun
        descriptions
         movement_type
         counterparty
         references
         exchange
         reported
         balance_after
         valuations
         names
         fields
       ->
       { descriptions
       ; movement_type
       ; counterparty
       ; references
       ; exchange
       ; reported
       ; balance_after
       ; valuations
       ; names
       ; fields
       })
  |> optional_list "descriptions" Jsont.string ~enc:(fun d -> d.descriptions)
  |> Jsont.Object.opt_mem "type" movement_type ~enc:(fun d -> d.movement_type)
  |> Jsont.Object.opt_mem "counterparty" counterparty_codec ~enc:(fun d -> d.counterparty)
  |> Jsont.Object.opt_mem "references" references_codec ~enc:(fun d -> d.references)
  |> Jsont.Object.opt_mem "exchange" exchange_codec ~enc:(fun d -> d.exchange)
  |> Jsont.Object.opt_mem "reported" reported_codec ~enc:(fun d -> d.reported)
  |> Jsont.Object.opt_mem "balance_after" quantity ~enc:(fun d -> d.balance_after)
  |> optional_list "valuations" amount_codec ~enc:(fun d -> d.valuations)
  |> Jsont.Object.mem
       "names"
       Json.string_map
       ~dec_absent:[]
       ~enc_omit:List.is_empty
       ~enc:(fun d -> d.names)
  |> optional_list "fields" field ~enc:(fun d -> d.fields)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let no_details =
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

let record_codec =
  Jsont.Object.map
    (fun
        id
         account
         statements
         occurred
         value_on
         movements
         fee
         group
         supporting
         details
         observation
         : record
       ->
       let supporting = Option.is_some supporting
       and observation = Option.is_some observation in
       if supporting && Option.is_none group then refuse "supporting requires a group";
       if supporting && observation
       then refuse "a record cannot be both supporting and an observation";
       { id
       ; account
       ; statements
       ; occurred
       ; value_on
       ; movements
       ; fee
       ; group
       ; supporting
       ; observation
       ; details
       })
  |> Jsont.Object.mem "id" text ~enc:(fun (r : record) -> r.id)
  |> Jsont.Object.mem "account" text ~enc:(fun (r : record) -> r.account)
  |> optional_list "statements" text ~enc:(fun (r : record) -> r.statements)
  |> Jsont.Object.mem "occurred" temporal ~enc:(fun r -> r.occurred)
  |> Jsont.Object.opt_mem "value_on" date ~enc:(fun r -> r.value_on)
  |> Jsont.Object.mem "movements" (Jsont.list movement_codec) ~enc:(fun r -> r.movements)
  |> Jsont.Object.opt_mem "fee" positive_quantity ~enc:(fun r -> r.fee)
  |> Jsont.Object.opt_mem "group" text ~enc:(fun r -> r.group)
  |> flag "supporting" ~enc:(fun r -> r.supporting)
  |> Jsont.Object.mem
       "details"
       details_codec
       ~dec_absent:no_details
       ~enc_omit:(fun d ->
         List.is_empty d.descriptions
         && Option.is_none d.movement_type
         && Option.is_none d.counterparty
         && Option.is_none d.references
         && Option.is_none d.exchange
         && Option.is_none d.reported
         && Option.is_none d.balance_after
         && List.is_empty d.valuations
         && List.is_empty d.fields)
       ~enc:(fun r -> r.details)
  |> flag "observation" ~enc:(fun r -> r.observation)
  |> Jsont.Object.finish
;;

let header_codec =
  Jsont.Object.map (fun () () institution identity converter : header ->
    { institution; identity; converter })
  |> Jsont.Object.mem "format" (Json.tag "fin-records") ~enc:(fun _ -> ())
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "institution" token ~enc:(fun (h : header) -> h.institution)
  |> Jsont.Object.mem "identity" identity_codec ~enc:(fun (h : header) -> h.identity)
  |> Jsont.Object.opt_mem "converter" converter_codec ~enc:(fun (h : header) ->
    h.converter)
  |> Jsont.Object.finish
;;

let count =
  Jsont.map
    Jsont.number
    ~dec:(fun n ->
      if Float.is_integer n && Float.(n >= 0.) && Float.(n <= 9007199254740991.)
      then Int.of_float n
      else refuse "expected a count")
    ~enc:Float.of_int
;;

let end_codec =
  Jsont.Object.map Fn.id
  |> Jsont.Object.mem "records" count ~enc:Fn.id
  |> Jsont.Object.finish
;;

type line =
  | Header of header
  | Source_line of source
  | Account_line of account
  | Statement_line of statement
  | Balance_line of balance
  | Record_line of record
  | End of int

let line_codec =
  let open Jsont.Object in
  let header = Case.map "header" header_codec ~dec:(fun x -> Header x)
  and source = Case.map "source" source_codec ~dec:(fun x -> Source_line x)
  and account = Case.map "account" account_codec ~dec:(fun x -> Account_line x)
  and statement = Case.map "statement" statement_codec ~dec:(fun x -> Statement_line x)
  and balance = Case.map "balance" balance_codec ~dec:(fun x -> Balance_line x)
  and record = Case.map "record" record_codec ~dec:(fun x -> Record_line x)
  and end_ = Case.map "end" end_codec ~dec:(fun x -> End x) in
  map Fn.id
  |> case_mem
       "kind"
       Jsont.string
       ~enc:Fn.id
       ~enc_case:(function
         | Header x -> Case.value header x
         | Source_line x -> Case.value source x
         | Account_line x -> Case.value account x
         | Statement_line x -> Case.value statement x
         | Balance_line x -> Case.value balance x
         | Record_line x -> Case.value record x
         | End x -> Case.value end_ x)
       Case.
         [ make header
         ; make source
         ; make account
         ; make statement
         ; make balance
         ; make record
         ; make end_
         ]
  |> error_unknown
  |> finish
;;

let row_codec =
  Jsont.Object.map (fun () account record -> account, record)
  |> Jsont.Object.mem "format" (Json.tag "fin-records-row") ~enc:(fun _ -> ())
  |> Jsont.Object.mem "account" account_codec ~enc:fst
  |> Jsont.Object.mem "record" record_codec ~enc:snd
  |> Jsont.Object.finish
;;

let row_to_string account record = Json.encode row_codec (account, record)
let row_of_string = Json.decode row_codec

let row_identity payload =
  match row_of_string payload with
  | Error _ -> None
  | Ok (account, record) ->
    Some
      (row_to_string
         { id = account.id
         ; bank = None
         ; number = None
         ; account_type = None
         ; currency = None
         ; iban = Unstated
         }
         { record with statements = []; details = no_details })
;;

let identity_text payload = Option.value (row_identity payload) ~default:payload

(* Each member of [details] on its own, so that one can be compared at a time. *)
let detail_members =
  let d = no_details in
  [ (fun (x : details) -> { d with descriptions = x.descriptions })
  ; (fun x -> { d with movement_type = x.movement_type })
  ; (fun x -> { d with counterparty = x.counterparty })
  ; (fun x -> { d with references = x.references })
  ; (fun x -> { d with exchange = x.exchange })
  ; (fun x -> { d with reported = x.reported })
  ; (fun x -> { d with balance_after = x.balance_after })
  ; (fun x -> { d with valuations = x.valuations })
  ; (fun x -> { d with names = x.names })
  ; (fun x -> { d with fields = x.fields })
  ]
;;

let restates ~stored payload =
  match row_identity stored, row_identity payload with
  | Some a, Some b ->
    String.equal a b
    &&
      (match row_of_string stored, row_of_string payload with
      | Ok (_, old), Ok (_, now) ->
        let text = Json.encode details_codec in
        let empty = text no_details in
        List.for_all detail_members ~f:(fun only ->
          let before = text (only old.details) in
          String.equal before empty || String.equal before (text (only now.details)))
      | _ -> false)
  | _ -> String.equal stored payload
;;

(* The first step of these rows, in this order, that does not add up. *)
let balance_break rows =
  let stated =
    List.filter_map rows ~f:(fun r ->
      match r.details.balance_after, r.movements with
      | Some balance, [ movement ] -> Some (r, balance, movement.quantity)
      | _ -> None)
  in
  let steps =
    match stated with
    | [] -> []
    | _ -> List.zip_exn (List.drop_last_exn stated) (List.drop stated 1)
  in
  List.find_map steps ~f:(fun ((before, previous, _), (r, balance, quantity)) ->
    if Date.( < ) (local_date r.occurred) (local_date before.occurred)
    then Some (Printf.sprintf "record %S of account %S is out of order" r.id r.account)
    else if Quantity.equal Quantity.(previous + quantity) balance
    then None
    else
      Some
        (Printf.sprintf
           "record %S of account %S states a balance of %s after %s and a movement of %s"
           r.id
           r.account
           (Quantity.to_string balance)
           (Quantity.to_string previous)
           (Quantity.to_string quantity)))
;;

let check_balances t =
  List.filter_map t.accounts ~f:(fun (account : account) ->
    let rows = List.filter t.records ~f:(fun r -> String.equal r.account account.id) in
    match balance_break rows, balance_break (List.rev rows) with
    | Some problem, Some _ -> Some problem
    | _ -> None)
;;

let check t =
  let problems = Queue.create () in
  let problem fmt = Printf.ksprintf (Queue.enqueue problems) fmt in
  if List.is_empty t.sources then problem "the section names no source";
  let ids what ids =
    let seen = Hash_set.create (module String) in
    List.iter ids ~f:(fun id ->
      if Hash_set.mem seen id then problem "%s %S is listed twice" what id;
      Hash_set.add seen id);
    seen
  in
  let accounts = ids "account" (List.map t.accounts ~f:(fun (a : account) -> a.id)) in
  let (_ : string Hash_set.t) =
    ids "statement" (List.map t.statements ~f:(fun (s : statement) -> s.id))
  in
  let statements =
    List.map t.statements ~f:(fun (s : statement) -> s.id, s.account)
    |> Hashtbl.of_alist_multi (module String)
  in
  List.iter t.statements ~f:(fun s ->
    if not (Hash_set.mem accounts s.account)
    then problem "statement %S: unknown account %S" s.id s.account);
  List.iter t.balances ~f:(fun b ->
    if not (Hash_set.mem accounts b.account)
    then problem "balance as of %s: unknown account %S" b.as_of.stated b.account);
  let record_ids = Hash_set.create (module String) in
  List.iter t.records ~f:(fun r ->
    let problem fmt =
      Printf.ksprintf (problem "record %S of account %S: %s" r.id r.account) fmt
    in
    if not (Hash_set.mem accounts r.account) then problem "unknown account";
    List.iter r.statements ~f:(fun statement ->
      match Hashtbl.find statements statement with
      | None -> problem "unknown statement %S" statement
      | Some owners ->
        if not (List.mem owners r.account ~equal:String.equal)
        then problem "statement %S belongs to another account" statement);
    Option.iter
      (List.find_a_dup r.statements ~compare:String.compare)
      ~f:(fun statement -> problem "lists statement %S twice" statement);
    let key = r.account ^ "\000" ^ r.id in
    if Hash_set.mem record_ids key
    then problem "repeated id: identical rows cannot be told apart";
    Hash_set.add record_ids key;
    (match
       List.find_a_dup
         (List.map r.movements ~f:(fun (m : movement) -> m.role))
         ~compare:String.compare
     with
     | Some role -> problem "repeated role %S" role
     | None -> ());
    let one_movement = Int.equal (List.length r.movements) 1 in
    if Option.is_some r.fee && not one_movement
    then problem "a fee needs exactly one movement";
    if Option.exists r.fee ~f:(fun fee -> Quantity.(fee < zero))
    then problem "a fee cannot be negative";
    if Option.is_some r.details.balance_after && not one_movement
    then problem "a balance after the row needs exactly one movement";
    if r.observation && not one_movement
    then problem "an observation needs exactly one movement";
    List.iter r.details.names ~f:(fun (symbol, _) ->
      if
        not
          (List.exists r.movements ~f:(fun (m : movement) ->
             match m.commodity with
             | Reported { symbol = reported; _ } -> String.equal reported symbol
             | Fiat _ | Asset _ -> false))
      then problem "names %S, which no movement reports" symbol));
  if List.length t.sources = 1
  then List.iter (check_balances t) ~f:(Queue.enqueue problems);
  if Queue.is_empty problems
  then Ok ()
  else Or_error.error_string (String.concat ~sep:"\n" (Queue.to_list problems))
;;

let merge documents =
  let open Or_error.Let_syntax in
  let%bind first =
    match documents with
    | first :: _ -> Ok first
    | [] -> Or_error.error_string "no documents to merge"
  in
  let%bind () =
    if
      List.for_all documents ~f:(fun t ->
        String.equal t.institution first.institution
        && Poly.equal t.identity first.identity)
    then Ok ()
    else Or_error.error_string "documents of different institutions or identities"
  in
  let problems = Queue.create () in
  (* What two files both state is one item, as long as they agree on it. *)
  let once describe codec ~key items =
    let seen = Hashtbl.create (module String) in
    List.filter items ~f:(fun item ->
      let text = Json.encode codec item in
      match Hashtbl.find seen (key item) with
      | None ->
        Hashtbl.set seen ~key:(key item) ~data:text;
        true
      | Some earlier ->
        if not (String.equal earlier text)
        then Queue.enqueue problems (describe item ^ " is stated differently by two files");
        false)
  in
  let all f = List.concat_map documents ~f in
  let merged =
    { first with
      sources =
        once
          (fun (s : source) -> Printf.sprintf "source %S" s.name)
          source_codec
          ~key:(fun (s : source) -> s.name ^ "\000" ^ s.sha256)
          (all (fun t -> t.sources))
    ; accounts =
        (* One file may know an account's IBAN and another only its number. *)
        (let known = Hashtbl.create (module String) in
         let order = Queue.create () in
         let either id what a b =
           match a, b with
           | Some a, Some b when not (String.equal a b) ->
             Queue.enqueue
               problems
               (Printf.sprintf
                  "account %S: %s is stated differently by two files"
                  id
                  what);
             Some a
           | Some a, _ -> Some a
           | None, b -> b
         in
         List.iter
           (all (fun t -> t.accounts))
           ~f:(fun (a : account) ->
             match Hashtbl.find known a.id with
             | None ->
               Queue.enqueue order a.id;
               Hashtbl.set known ~key:a.id ~data:a
             | Some (b : account) ->
               let id = a.id in
               let iban =
                 match b.iban, a.iban with
                 | Unstated, iban | iban, Unstated -> iban
                 | Iban x, Iban y when not (String.equal x y) ->
                   Queue.enqueue
                     problems
                     (Printf.sprintf
                        "account %S: iban is stated differently by two files"
                        id);
                   b.iban
                 | Iban _, No_iban | No_iban, Iban _ ->
                   Queue.enqueue
                     problems
                     (Printf.sprintf
                        "account %S: iban is stated differently by two files"
                        id);
                   b.iban
                 | iban, _ -> iban
               in
               let currency =
                 either
                   id
                   "currency"
                   (Option.map b.currency ~f:Iso4217.to_alpha_code)
                   (Option.map a.currency ~f:Iso4217.to_alpha_code)
               in
               Hashtbl.set
                 known
                 ~key:id
                 ~data:
                   { id
                   ; bank = either id "bank" b.bank a.bank
                   ; number = either id "number" b.number a.number
                   ; account_type = either id "type" b.account_type a.account_type
                   ; currency =
                       Option.bind currency ~f:(fun code ->
                         Validation.currency "currency" code |> Result.ok)
                   ; iban
                   });
         Queue.to_list order |> List.map ~f:(Hashtbl.find_exn known))
    ; statements =
        once
          (fun (s : statement) -> Printf.sprintf "statement %S" s.id)
          statement_codec
          ~key:(fun (s : statement) -> s.id)
          (all (fun t -> t.statements))
    ; balances =
        once
          (fun (b : balance) -> Printf.sprintf "balance as of %s" b.as_of.stated)
          balance_codec
          ~key:(fun (b : balance) ->
            String.concat
              ~sep:"\000"
              [ b.account
              ; Json.encode balance_type_codec b.balance_type
              ; Commodity.display b.commodity
              ; b.as_of.stated
              ])
          (all (fun t -> t.balances))
    ; records =
        (let records = all (fun t -> t.records) in
         let key (r : record) = r.account ^ "\000" ^ r.id in
         let listed = Hashtbl.create (module String) in
         List.iter records ~f:(fun (r : record) ->
           Hashtbl.update listed (key r) ~f:(fun known ->
             let known = Option.value known ~default:[] in
             known
             @ List.filter r.statements ~f:(fun id ->
               not (List.mem known id ~equal:String.equal))));
         (* Exports name their statements differently, so one row can be listed by
            several. That is no disagreement about the row. *)
         once
           (fun (r : record) -> Printf.sprintf "record %S of account %S" r.id r.account)
           record_codec
           ~key
           (List.map records ~f:(fun (r : record) -> { r with statements = [] }))
         |> List.map ~f:(fun (r : record) ->
           { r with statements = Hashtbl.find_exn listed (key r) }))
    }
  in
  if Queue.is_empty problems
  then Ok merged
  else Or_error.error_string (String.concat ~sep:"\n" (Queue.to_list problems))
;;

let merge_all documents =
  let open Or_error.Let_syntax in
  (* A row one file states twice is not an overlap: merging would silently lose one. *)
  let%bind () = List.map documents ~f:check |> Or_error.all_unit in
  let same (a : t) (b : t) =
    String.equal a.institution b.institution && Poly.equal a.identity b.identity
  in
  (* Sections that can be one document are: this is what a concatenated overlap needs, and
     it is where they are compared. *)
  List.fold documents ~init:[] ~f:(fun groups t ->
    if List.exists groups ~f:(same t) then groups else t :: groups)
  |> List.rev_map ~f:(fun group -> List.filter documents ~f:(same group) |> merge)
  |> Or_error.all
;;

let zstd_magic = "\x28\xB5\x2F\xFD"

let stream_result f =
  try Ok (f ()) with
  | Bytes_stream.Stream.Error error ->
    Or_error.error_string ("zstd: " ^ Bytes_stream.Stream.error_message error)
;;

let compress text =
  let params = Bytesrw_zstd.Cctx_params.make ~checksum:true () in
  stream_result (fun () ->
    Bytes_stream.Reader.of_string text
    |> Bytesrw_zstd.compress_reads ~params ()
    |> Bytes_stream.Reader.to_string)
;;

let decompressed contents =
  if String.is_prefix contents ~prefix:zstd_magic
  then
    stream_result (fun () ->
      Bytes_stream.Reader.of_string contents
      |> Bytesrw_zstd.decompress_reads ()
      |> Bytes_stream.Reader.to_string)
  else Ok contents
;;

(* A file is sections, each from its header to its end line, so that files can be
   concatenated. The end line is how a file cut between two lines is noticed. *)
let sections text =
  let open Or_error.Let_syntax in
  let lines =
    match List.rev (String.split text ~on:'\n') with
    | "" :: rest -> List.rev rest
    | _ -> String.split text ~on:'\n'
  in
  let at number message = Or_error.errorf "line %d: %s" number message in
  let add open_ line =
    match open_, line with
    | Some t, Source_line s -> Some { t with sources = s :: t.sources }
    | Some t, Account_line a -> Some { t with accounts = a :: t.accounts }
    | Some t, Statement_line s -> Some { t with statements = s :: t.statements }
    | Some t, Balance_line b -> Some { t with balances = b :: t.balances }
    | Some t, Record_line r -> Some { t with records = r :: t.records }
    | _ -> open_
  in
  let%bind open_, closed, _ =
    List.fold_result lines ~init:(None, [], 1) ~f:(fun (open_, closed, number) text ->
      let%bind line =
        Json.decode line_codec text
        |> Result.map_error ~f:(fun error ->
          Error.of_string
            (Printf.sprintf "line %d: %s" number (Error.to_string_hum error)))
      in
      match open_, line with
      | Some _, Header _ -> at number "a header before the previous section's end line"
      | None, Header (h : header) ->
        Ok
          ( Some
              { institution = h.institution
              ; identity = h.identity
              ; converter = h.converter
              ; sources = []
              ; accounts = []
              ; statements = []
              ; balances = []
              ; records = []
              }
          , closed
          , number + 1 )
      | None, _ -> at number "expected a header"
      | Some t, End stated ->
        let found = List.length t.records in
        if stated <> found
        then
          at
            number
            (Printf.sprintf "the end line counts %d records, found %d" stated found)
        else Ok (None, t :: closed, number + 1)
      | Some _, _ -> Ok (add open_ line, closed, number + 1))
  in
  match open_, closed with
  | Some _, _ -> Or_error.error_string "the file has no end line; it may be truncated"
  | None, [] -> Or_error.error_string "the file is empty"
  | None, closed ->
    Ok
      (List.rev_map closed ~f:(fun t ->
         { t with
           sources = List.rev t.sources
         ; accounts = List.rev t.accounts
         ; statements = List.rev t.statements
         ; balances = List.rev t.balances
         ; records = List.rev t.records
         }))
;;

let is_records_of ~institution contents =
  let first_line text = List.hd_exn (String.split text ~on:'\n') in
  match decompressed contents with
  | Error _ -> false
  | Ok text ->
    (match Json.decode line_codec (first_line text) with
     | Ok (Header header) -> String.equal header.institution institution
     | Ok _ | Error _ -> false)
;;

let of_string ?file contents =
  let open Or_error.Let_syntax in
  let result =
    let%bind text = decompressed contents in
    let%bind sections = sections text in
    merge_all sections
  in
  Result.map_error result ~f:(fun error ->
    match file with
    | None -> error
    | Some file -> Error.of_string (file ^ ": " ^ Error.to_string_hum error))
;;

let to_string t =
  let header =
    Header { institution = t.institution; identity = t.identity; converter = t.converter }
  in
  [ [ header ]
  ; List.map t.sources ~f:(fun x -> Source_line x)
  ; List.map t.accounts ~f:(fun x -> Account_line x)
  ; List.map t.statements ~f:(fun x -> Statement_line x)
  ; List.map t.balances ~f:(fun x -> Balance_line x)
  ; List.map t.records ~f:(fun x -> Record_line x)
  ; [ End (List.length t.records) ]
  ]
  |> List.concat
  |> List.map ~f:(fun line -> Json.encode line_codec line ^ "\n")
  |> String.concat
;;
