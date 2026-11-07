open Base
open Stdio
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let date = Option.value_map ~default:"-" ~f:Date.to_string

let decision_text = function
  | Assets.Unmapped -> "unmapped"
  | Ignored -> "ignored"
  | Mapped commodity -> Commodity.display commodity
  | Assumed (Fiat currency) -> Iso4217.to_alpha_code currency ^ " (fiat, assumed)"
  | Assumed commodity -> Commodity.display commodity ^ " (assumed)"
;;

let asset_json =
  let date =
    Jsont.option (Jsont.map Jsont.string ~dec:Date.of_string ~enc:Date.to_string)
  in
  Jsont.Object.enc_only ()
  |> Jsont.Object.mem "provider" Jsont.string ~enc:(fun (a : Assets.t) -> a.provider)
  |> Jsont.Object.mem "symbol" Jsont.string ~enc:(fun (a : Assets.t) -> a.symbol)
  |> Jsont.Object.mem "name" (Jsont.option Jsont.string) ~enc:(fun (a : Assets.t) ->
    a.name)
  |> Jsont.Object.mem "movements" Jsont.int ~enc:(fun (a : Assets.t) -> a.movements)
  |> Jsont.Object.mem "first_seen" date ~enc:(fun (a : Assets.t) -> a.first_seen)
  |> Jsont.Object.mem "last_seen" date ~enc:(fun (a : Assets.t) -> a.last_seen)
  |> Jsont.Object.mem "decision" Jsont.string ~enc:(fun (a : Assets.t) ->
    match a.decision with
    | Unmapped -> "unmapped"
    | Ignored -> "ignored"
    | Mapped _ -> "mapped"
    | Assumed _ -> "assumed")
  |> Jsont.Object.mem
       "commodity"
       (Jsont.option Commodity.codec)
       ~enc:(fun (a : Assets.t) ->
         match a.decision with
         | Mapped c -> Some c
         | Assumed commodity -> Some commodity
         | Unmapped | Ignored -> None)
  |> Jsont.Object.finish
;;

let asset_list database json_output =
  with_database database (fun db ->
    let%map assets = Assets.list db in
    if json_output
    then Report_json.print (Jsont.list asset_json) assets
    else if List.is_empty assets
    then print_endline "No reported symbols: every movement is in fiat or a known asset."
    else (
      (* Only a source with opaque symbols, such as token contracts, states names. *)
      let named = List.exists assets ~f:(fun (a : Assets.t) -> Option.is_some a.name) in
      let name_column f = if named then [ f ] else [] in
      Terminal.table
        ([ "Provider"; "Symbol" ]
         @ name_column "Name"
         @ [ "Movements"; "From"; "To"; "Is" ])
        (List.map assets ~f:(fun (a : Assets.t) ->
           [ a.provider; a.symbol ]
           @ name_column (Option.value a.name ~default:"-")
           @ [ Int.to_string a.movements
             ; date a.first_seen
             ; date a.last_seen
             ; decision_text a.decision
             ]));
      if List.exists assets ~f:(fun a -> Poly.equal a.decision Unmapped)
      then
        print_endline
          "\n\
           An unmapped symbol's movements count in no commodity. Map it with fin asset \
           map PROVIDER SYMBOL CODE once you know what it is, such as from its token \
           contract, or fin asset ignore PROVIDER SYMBOL if it is spam."))
;;

let provider_arg =
  Arg.(
    required
    & pos 0 (some string) None
    & info
        []
        ~docv:"PROVIDER"
        ~doc:"Who reports the symbol: a network such as polygon, or an exchange.")
;;

let symbol_arg =
  Arg.(
    required
    & pos 1 (some string) None
    & info
        []
        ~docv:"SYMBOL"
        ~doc:
          "The symbol as fin asset list prints it, or its unique start, such as the \
           first characters of a token contract.")
;;

let decide database provider symbol decision yes ~question =
  with_database database (fun db ->
    let%bind asset = Assets.find db ~provider ~symbol in
    printf
      "%s %s%s: %d movements, now %s.\n"
      asset.provider
      asset.symbol
      (Option.value_map asset.name ~default:"" ~f:(fun n -> " (" ^ n ^ ")"))
      asset.movements
      (decision_text asset.decision);
    let%bind confirmed = confirm ~yes ~destructive:false question in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map changed = Assets.decide db asset decision in
      print_endline (if changed then "Recorded." else "Nothing changed.")))
;;

let yes = Arg.(value & flag & info [ "yes" ] ~doc:"Confirm without prompting.")

let asset_map database provider symbol code yes =
  let%bind commodity = Commodity.of_code code in
  decide
    database
    provider
    symbol
    (Mapped commodity)
    yes
    ~question:(Printf.sprintf "Map it to %s?" (Commodity.display commodity))
;;

(* What a token list says of one of your symbols. *)
type finding =
  | Adopt of Assets.t * Finances.Token_list.token * Commodity.t
  | Unusable of Assets.t * Finances.Token_list.token * string
  | Disagrees of Assets.t * Finances.Token_list.token * Commodity.t
  | Ignored_listed of Assets.t * Finances.Token_list.token
  | Agrees

let findings ~chain_ids (list : Finances.Token_list.t) assets =
  let find = Finances.Token_list.lookup list in
  List.filter_map assets ~f:(fun (a : Assets.t) ->
    let%bind.Option chain_id = List.Assoc.find chain_ids a.provider ~equal:String.equal in
    let%map.Option token = find ~chain_id ~address:a.symbol in
    match a.decision, Commodity.of_code token.symbol with
    | (Unmapped | Assumed _), Ok commodity -> Adopt (a, token, commodity)
    | (Unmapped | Assumed _), Error error -> Unusable (a, token, Error.to_string_hum error)
    | Mapped mine, Ok listed when Commodity.equal mine listed -> Agrees
    | Mapped mine, _ -> Disagrees (a, token, mine)
    | Ignored, _ -> Ignored_listed (a, token))
;;

let asset_adopt database path yes =
  let%bind text = read_file path in
  let%bind list = Finances.Token_list.of_string text in
  let%bind stored = institution_definitions database in
  let chain_ids =
    List.filter_map (Builtin.definitions @ stored) ~f:(fun d ->
      Option.map (Institution_definition.chain_id d) ~f:(fun id ->
        Institution_definition.institution d, id))
  in
  let source = Printf.sprintf "%s %s" list.name list.version in
  with_database database (fun db ->
    let%bind assets = Assets.list db in
    let found = findings ~chain_ids list assets in
    let adopt =
      List.filter_map found ~f:(function
        | Adopt (a, t, c) -> Some (a, t, c)
        | _ -> None)
    in
    printf
      "%s lists %d tokens; %d of your symbols are among them, %d already mapped alike.\n"
      source
      (List.length list.tokens)
      (List.length found)
      (List.count found ~f:(function
         | Agrees -> true
         | _ -> false));
    List.iter found ~f:(function
      | Disagrees (a, t, mine) ->
        printf
          "  %s %s: you mapped it to %s, the list says %s; left as it is.\n"
          a.provider
          a.symbol
          (Commodity.display mine)
          t.symbol
      | Ignored_listed (a, t) ->
        printf
          "  %s %s: you ignored it, the list says %s (%s); left as it is.\n"
          a.provider
          a.symbol
          t.symbol
          t.name
      | Unusable (a, t, why) ->
        printf
          "  %s %s: the list says %s, which cannot be an asset: %s. Map it with fin \
           asset map.\n"
          a.provider
          a.symbol
          t.symbol
          why
      | Adopt _ | Agrees -> ());
    if List.is_empty adopt
    then (
      print_endline "Nothing to map.";
      Ok ())
    else (
      Terminal.table
        [ "Provider"; "Symbol"; "List says"; "Name"; "Movements" ]
        (List.map
           adopt
           ~f:(fun ((a : Assets.t), (t : Finances.Token_list.token), commodity) ->
             [ a.provider
             ; a.symbol
             ; Commodity.display commodity
             ; t.name
             ; Int.to_string a.movements
             ]));
      let%bind confirmed =
        confirm
          ~yes
          ~destructive:false
          (Printf.sprintf "Map these %d symbols as the list says?" (List.length adopt))
      in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%map changed =
          List.map adopt ~f:(fun (a, _, commodity) ->
            Assets.decide ~source db a (Mapped commodity))
          |> Or_error.all
        in
        printf "Recorded %d, from %s.\n" (List.count changed ~f:Fn.id) source)))
;;

(* Every symbol still unmapped, such as spam tokens once the real ones are mapped. *)
let asset_ignore_unmapped database provider yes =
  with_database database (fun db ->
    let%bind assets = Assets.list db in
    let unmapped =
      List.filter assets ~f:(fun (a : Assets.t) ->
        Poly.equal a.decision Unmapped
        && Option.for_all provider ~f:(String.equal a.provider))
    in
    if List.is_empty unmapped
    then (
      print_endline "No unmapped symbols.";
      Ok ())
    else (
      Terminal.table
        [ "Provider"; "Symbol"; "Name"; "Movements" ]
        (List.map unmapped ~f:(fun (a : Assets.t) ->
           [ a.provider
           ; a.symbol
           ; Option.value a.name ~default:"-"
           ; Int.to_string a.movements
           ]));
      let%bind confirmed =
        confirm
          ~yes
          ~destructive:false
          (Printf.sprintf
             "Ignore these %d symbols? Their movements stay and never count; fin asset \
              unmap undoes it."
             (List.length unmapped))
      in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%map changed =
          List.map unmapped ~f:(fun a -> Assets.decide db a Ignored) |> Or_error.all
        in
        printf "Ignored %d.\n" (List.count changed ~f:Fn.id))))
;;

let cmd =
  group
    "asset"
    "Say what each provider's symbol is: a token contract, or an exchange's ticker."
    [ command
        "list"
        "List the symbols providers reported, and what fin maps each to."
        Term.(
          const asset_list
          $ database
          $ Arg.(value & flag & info [ "json" ] ~doc:"Print JSON."))
    ; command
        "map"
        "Map a symbol to fiat or an asset, for every movement reporting it."
        ~man:
          [ `S Manpage.s_description
          ; `P
              "A token is known by its contract on a network, which anyone can name \
               USDC; map only a contract you have checked, such as on the issuer's site. \
               An asset is one asset whichever network holds it."
          ]
        Term.(
          const asset_map
          $ database
          $ provider_arg
          $ symbol_arg
          $ Arg.(
              required
              & pos 2 (some string) None
              & info
                  []
                  ~docv:"CODE"
                  ~doc:"An ISO currency code such as EUR, or an asset such as USDC.")
          $ yes)
    ; command
        "adopt"
        "Map your token contracts as a token list says, after one confirmation."
        ~man:
          [ `S Manpage.s_description
          ; `P
              "A token list says which contract on which chain is which token, as \
               wallets and exchanges such as CoW Swap or Uniswap publish them \
               (tokenlists.org). For each unmapped contract the list names, on a network \
               whose definition gives its chain, fin proposes the list's symbol as the \
               asset; one confirmation maps them all, recorded as coming from the list. \
               What you mapped or ignored is never changed. A contract the list does not \
               name is not thereby spam: lists are short."
          ]
        Term.(
          const asset_adopt
          $ database
          $ Arg.(
              required
              & pos 0 (some string) None
              & info [] ~docv:"LIST" ~doc:"A token list, in its JSON format.")
          $ yes)
    ; command
        "ignore"
        "Ignore a symbol, such as a spam token: its movements stay and never count."
        Term.(
          const (fun database provider symbol yes ->
            decide database provider symbol Ignored yes ~question:"Ignore it?")
          $ database
          $ provider_arg
          $ symbol_arg
          $ yes)
    ; command
        "ignore-unmapped"
        "Ignore every symbol still unmapped, such as spam tokens, after one confirmation."
        ~man:
          [ `S Manpage.s_description
          ; `P
              "Once the tokens you hold are mapped, by hand or with fin asset adopt, \
               what is left unmapped is usually spam. Symbols assumed to be a currency, \
               an exchange's ticker or an institution's own unit are not unmapped and \
               are left alone."
          ]
        Term.(
          const asset_ignore_unmapped
          $ database
          $ Arg.(
              value
              & pos 0 (some string) None
              & info
                  []
                  ~docv:"PROVIDER"
                  ~doc:"Only this provider's symbols, such as polygon.")
          $ yes)
    ; command
        "unmap"
        "Remove what a symbol was mapped to, or that it was ignored."
        Term.(
          const (fun database provider symbol yes ->
            decide database provider symbol Unmapped yes ~question:"Remove the mapping?")
          $ database
          $ provider_arg
          $ symbol_arg
          $ yes)
    ]
;;
