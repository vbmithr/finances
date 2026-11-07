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
  | Assumed_fiat currency -> Iso4217.to_alpha_code currency ^ " (fiat, assumed)"
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
    | Assumed_fiat _ -> "assumed")
  |> Jsont.Object.mem
       "commodity"
       (Jsont.option Commodity.codec)
       ~enc:(fun (a : Assets.t) ->
         match a.decision with
         | Mapped c -> Some c
         | Assumed_fiat currency -> Some (Commodity.fiat currency)
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
