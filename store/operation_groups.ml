open Base

type t =
  { others : int64 list Map.M(Int64).t
  ; canonical : int64 Map.M(Int64).t
  }

let empty = { others = Map.empty (module Int64); canonical = Map.empty (module Int64) }

let of_links links =
  List.fold_result links ~init:empty ~f:(fun t (first, second) ->
    if
      Int64.equal first second
      || Map.mem t.canonical first
      || Map.mem t.canonical second
      || Map.mem t.others first
      || Map.mem t.others second
    then
      Or_error.errorf "operation %Ld or %Ld belongs to more than one group" first second
    else
      Ok
        { others = Map.set t.others ~key:first ~data:[ second ]
        ; canonical = Map.set t.canonical ~key:second ~data:first
        })
;;

let canonical t id = Option.value (Map.find t.canonical id) ~default:id

let members t id =
  let first = canonical t id in
  first :: Option.value (Map.find t.others first) ~default:[]
;;
