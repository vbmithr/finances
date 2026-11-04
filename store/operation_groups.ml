open Base

type t =
  { others : int64 list Map.M(Int64).t
  ; canonical : int64 Map.M(Int64).t
  }

let empty = { others = Map.empty (module Int64); canonical = Map.empty (module Int64) }
let grouped t id = Map.mem t.canonical id || Map.mem t.others id

let add t first others =
  { others = Map.set t.others ~key:first ~data:others
  ; canonical =
      List.fold others ~init:t.canonical ~f:(fun canonical id ->
        Map.set canonical ~key:id ~data:first)
  }
;;

let create ~links ~stated =
  let open Or_error.Let_syntax in
  let%map linked =
    List.fold_result links ~init:empty ~f:(fun t (first, second) ->
      if Int64.equal first second || grouped t first || grouped t second
      then
        Or_error.errorf "operation %Ld or %Ld belongs to more than one group" first second
      else Ok (add t first [ second ]))
  in
  List.fold stated ~init:linked ~f:(fun t group ->
    match List.filter group ~f:(fun id -> not (grouped linked id)) with
    | first :: (_ :: _ as others) -> add t first others
    | _ -> t)
;;

let canonical t id = Option.value (Map.find t.canonical id) ~default:id
let noncanonical_members t = Map.to_alist t.canonical

let members t id =
  let first = canonical t id in
  first :: Option.value (Map.find t.others first) ~default:[]
;;
