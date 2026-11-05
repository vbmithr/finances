open Base

type link =
  { first : Category_portable.decision
  ; second : Category_portable.decision
  ; reason : string
  ; projection_sha256 : string option
  }

type t = { links : link list }

let link =
  Jsont.Object.map (fun first second reason projection_sha256 ->
    { first; second; reason; projection_sha256 })
  |> Jsont.Object.mem "first" Category_portable.decision ~enc:(fun r -> r.first)
  |> Jsont.Object.mem "second" Category_portable.decision ~enc:(fun r -> r.second)
  |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.opt_mem "projection_sha256" Jsont.string ~enc:(fun r ->
    r.projection_sha256)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let codec =
  Jsont.Object.map (fun () links -> { links })
  |> Jsont.Object.mem "version" (Json.version 2) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "links" (Jsont.list link) ~enc:(fun r -> r.links)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;
