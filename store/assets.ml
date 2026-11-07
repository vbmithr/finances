open Base
open Rel
open Handle

type decision =
  | Unmapped
  | Mapped of Commodity.t
  | Ignored
  | Assumed_fiat of Iso4217.t

type t =
  { provider : string
  ; symbol : string
  ; movements : int
  ; first_seen : Date.t option
  ; last_seen : Date.t option
  ; name : string option
  ; decision : decision
  }

let decision_of ~decided ~symbol ~currency commodity =
  match decided, commodity with
  | false, _ when currency ->
    Or_error.of_option
      (Option.map (Iso4217.of_alpha_code symbol) ~f:(fun c -> Assumed_fiat c))
      ~error:(Error.createf "%s is not an ISO 4217 code" symbol)
  | false, _ -> Ok Unmapped
  | true, None -> Ok Ignored
  | true, Some json ->
    let* commodity = Json.decode Commodity.codec json in
    Ok (Mapped commodity)
;;

let list db =
  let* rows =
    collect
      db
      (statement
         Sql.asset_list
         Row.(
           unit (fun a b c d e f g h i -> a, b, c, d, e, f, g, h, i)
           * text "provider"
           * text "symbol"
           * int "movements"
           * option Rel.Type.text "first_seen"
           * option Rel.Type.text "last_seen"
           * option Rel.Type.text "name"
           * bool "decided"
           * option Rel.Type.text "commodity"
           * bool "currency"))
  in
  let date = Option.map ~f:Date.of_string in
  traverse
    (fun ( provider
         , symbol
         , movements
         , first_seen
         , last_seen
         , name
         , decided
         , commodity
         , currency ) ->
       let* decision = decision_of ~decided ~symbol ~currency commodity in
       Ok
         { provider
         ; symbol
         ; movements
         ; first_seen = date first_seen
         ; last_seen = date last_seen
         ; name
         ; decision
         })
    rows
;;

let find db ~provider ~symbol =
  let* assets = list db in
  let ours = List.filter assets ~f:(fun a -> String.equal a.provider provider) in
  match List.find ours ~f:(fun a -> String.equal a.symbol symbol) with
  | Some asset -> Ok asset
  | None ->
    (match
       List.filter ours ~f:(fun a ->
         String.is_prefix (String.lowercase a.symbol) ~prefix:(String.lowercase symbol))
     with
     | [ asset ] -> Ok asset
     | [] -> Or_error.errorf "no symbol %s from %s; see fin asset list" symbol provider
     | _ ->
       Or_error.errorf "several symbols from %s start with %s; give more" provider symbol)
;;

(* The confirmed operations that read a symbol's commodity: changing it would change them. *)
let confirmed_uses db ~provider ~symbol =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT count(*) FROM movement_claims c JOIN operation_movements m ON m.id=c.id \
         WHERE m.commodity->>'kind'='reported' AND m.commodity->>'provider'=? AND \
         m.commodity->>'code'=? AND (c.linked OR c.exchanged)"
        (text @-> text @-> ret integer))
  in
  let* count = db_result (Db.first db.conn (query provider symbol)) in
  Ok (Option.value count ~default:0)
;;

let write db ~provider ~symbol ~previous next =
  let encode = Option.map ~f:(Json.encode Commodity.codec) in
  let set, action =
    match next with
    | Unmapped ->
      ( Rel_sql.Stmt.(
          func
            "DELETE FROM asset_mappings WHERE provider=? AND symbol=?"
            (text @-> text @-> unit))
          provider
          symbol
      , "removed" )
    | Mapped _ | Ignored | Assumed_fiat _ ->
      let commodity =
        match next with
        | Mapped c -> Some c
        | _ -> None
      in
      ( Rel_sql.Stmt.(
          func
            "INSERT INTO asset_mappings(provider,symbol,commodity) VALUES (?,?,?) ON \
             CONFLICT(provider,symbol) DO UPDATE SET commodity=excluded.commodity"
            (text @-> text @-> option text @-> unit))
          provider
          symbol
          (encode commodity)
      , if Option.is_some commodity then "mapped" else "ignored" )
  in
  let audit =
    Rel_sql.Stmt.(
      func
        "INSERT INTO asset_mapping_changes(provider,symbol,previous,commodity,action) \
         VALUES (?,?,?,?,?)"
        (text @-> text @-> option text @-> option text @-> text @-> unit))
  in
  let commodity_of = function
    | Mapped c -> Some c
    | Unmapped | Ignored | Assumed_fiat _ -> None
  in
  let* () = db_result (Db.exec db.conn set) in
  db_result
    (Db.exec
       db.conn
       (audit
          provider
          symbol
          (encode (commodity_of previous))
          (encode (commodity_of next))
          action))
;;

let same a b =
  match a, b with
  | Unmapped, Unmapped | Ignored, Ignored -> true
  | Assumed_fiat a, Assumed_fiat b -> Iso4217.equal a b
  | Mapped a, Mapped b -> Commodity.equal a b
  | _ -> false
;;

let decide db (asset : t) decision =
  let* () =
    match decision with
    | Mapped (Commodity.Reported _) ->
      Or_error.error_string "map a symbol to fiat or an asset, such as EUR or USDC"
    | _ -> Ok ()
  in
  if same asset.decision decision
  then Ok false
  else
    transaction db (fun db ->
      let* uses = confirmed_uses db ~provider:asset.provider ~symbol:asset.symbol in
      if uses > 0
      then
        Or_error.errorf
          "%d confirmed movements use %s; undo their transfers or exchanges first"
          uses
          asset.symbol
      else
        let* () =
          write
            db
            ~provider:asset.provider
            ~symbol:asset.symbol
            ~previous:asset.decision
            decision
        in
        Ok true)
;;

let mappings db =
  let* assets = list db in
  Ok
    (List.filter_map assets ~f:(fun a ->
       match a.decision with
       | Unmapped | Assumed_fiat _ -> None
       | Mapped c -> Some (a.provider, a.symbol, Some c)
       | Ignored -> Some (a.provider, a.symbol, None)))
;;

let restore db ~provider ~symbol commodity =
  let* () =
    match commodity with
    | Some (Commodity.Reported _) ->
      Or_error.errorf "%s %s is mapped to another provider's symbol" provider symbol
    | _ -> Ok ()
  in
  let wanted =
    match commodity with
    | Some c -> Mapped c
    | None -> Ignored
  in
  let* assets = list db in
  let current =
    List.find assets ~f:(fun a ->
      String.equal a.provider provider && String.equal a.symbol symbol)
    |> Option.value_map ~default:Unmapped ~f:(fun a -> a.decision)
  in
  match current with
  | Unmapped | Assumed_fiat _ ->
    let* () = write db ~provider ~symbol ~previous:Unmapped wanted in
    Ok true
  | current when same current wanted -> Ok false
  | _ -> Or_error.errorf "%s %s is already mapped differently" provider symbol
;;
