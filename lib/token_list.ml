open Base

type token =
  { chain_id : int
  ; address : string
  ; symbol : string
  ; name : string
  }

type t =
  { name : string
  ; version : string
  ; tokens : token list
  }

let token =
  Jsont.Object.map (fun chain_id address symbol name : token ->
    { chain_id; address = String.lowercase address; symbol; name })
  |> Jsont.Object.mem "chainId" Jsont.int ~enc:(fun (t : token) -> t.chain_id)
  |> Jsont.Object.mem "address" Jsont.string ~enc:(fun (t : token) -> t.address)
  |> Jsont.Object.mem "symbol" Jsont.string ~enc:(fun (t : token) -> t.symbol)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun (t : token) -> t.name)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish
;;

let version =
  Jsont.Object.map (fun major minor patch -> Printf.sprintf "%d.%d.%d" major minor patch)
  |> Jsont.Object.mem "major" Jsont.int ~enc:(fun _ -> 0)
  |> Jsont.Object.mem "minor" Jsont.int ~enc:(fun _ -> 0)
  |> Jsont.Object.mem "patch" Jsont.int ~enc:(fun _ -> 0)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish
;;

let codec =
  Jsont.Object.map (fun name version tokens : t -> { name; version; tokens })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun (t : t) -> t.name)
  |> Jsont.Object.mem "version" version ~enc:(fun (t : t) -> t.version)
  |> Jsont.Object.mem "tokens" (Jsont.list token) ~enc:(fun (t : t) -> t.tokens)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish
;;

let of_string = Json.decode codec

let lookup t =
  let index =
    Hashtbl.of_alist_multi
      (module String)
      (List.map t.tokens ~f:(fun (token : token) ->
         Printf.sprintf "%d:%s" token.chain_id token.address, token))
  in
  fun ~chain_id ~address ->
    match
      Hashtbl.find index (Printf.sprintf "%d:%s" chain_id (String.lowercase address))
    with
    | Some (token :: _) -> Some token
    | Some [] | None -> None
;;
