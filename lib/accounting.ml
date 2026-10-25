open Core

type posting =
  { account : string
  ; commodity : string
  ; quantity : Bigdecimal.t
  }

type entry =
  { date : Date.t
  ; description : string
  ; postings : posting list
  }

let posting ~account ~commodity quantity =
  let open Or_error.Let_syntax in
  let%bind account = Validation.account account in
  let%map commodity = Validation.token "commodity" commodity in
  { account; commodity; quantity }
;;

let entry ~date ~description postings =
  let open Or_error.Let_syntax in
  let%bind description = Validation.text "description" description in
  if List.length postings < 2
  then Or_error.error_string "an entry needs at least two postings"
  else (
    let sums = String.Table.create () in
    List.iter postings ~f:(fun p ->
      Hashtbl.update sums p.commodity ~f:(fun sum ->
        Bigdecimal.(Option.value sum ~default:zero + p.quantity)));
    if Hashtbl.for_all sums ~f:Bigdecimal.is_zero
    then Ok { date; description; postings }
    else Or_error.error_string "postings must balance exactly in every commodity")
;;
