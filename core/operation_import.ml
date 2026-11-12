open Base

type leg =
  { role : string
  ; account : string option
  ; commodity : Commodity.t
  ; quantity : Quantity.t
  }

type row =
  { source : Source_event.t
  ; legs : leg list
  }

let leg ~account ~role ~commodity ~quantity =
  let open Or_error.Let_syntax in
  let%bind role = Validation.token "movement role" role in
  let%map quantity = Or_error.try_with (fun () -> Quantity.finite_exn quantity) in
  { role; account; commodity; quantity }
;;

let row ~source ~legs =
  if List.contains_dup (List.map legs ~f:(fun leg -> leg.role)) ~compare:String.compare
  then Or_error.error_string "duplicate movement roles in a source row"
  else Ok { source; legs }
;;
