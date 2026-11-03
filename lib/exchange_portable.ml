open Base

type exchange =
  { source : Category_portable.decision
  ; projection_sha256 : string
  }

type t = { exchanges : exchange list }

let exchange =
  Jsont.Object.map (fun source projection_sha256 -> { source; projection_sha256 })
  |> Jsont.Object.mem "source" (Category_portable.decision ~legacy:false) ~enc:(fun e ->
    e.source)
  |> Jsont.Object.mem "projection_sha256" Jsont.string ~enc:(fun e -> e.projection_sha256)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let codec =
  Jsont.Object.map (fun () exchanges -> { exchanges })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "exchanges" (Jsont.list exchange) ~enc:(fun t -> t.exchanges)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;
