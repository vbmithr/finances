open Base

type t =
  { event_id : int64
  ; account : string
  ; reason : string
  }

let create ~event_id ~account ~reason =
  let open Or_error.Let_syntax in
  let%bind account = Validation.account account in
  let%bind reason = Validation.text "category reason" (String.strip reason) in
  if Int64.(event_id <= 0L)
  then Or_error.error_string "event_id must be positive"
  else if
    not
      (String.equal account "equity:unclassified"
       || String.is_prefix account ~prefix:"expenses:"
       || String.is_prefix account ~prefix:"income:")
  then
    Or_error.error_string
      "category account must be expenses:..., income:..., or equity:unclassified"
  else Ok { event_id; account; reason }
;;

let of_json contents =
  Or_error.try_with (fun () ->
    let open Yojson.Safe.Util in
    Yojson.Safe.from_string contents
    |> to_list
    |> List.map ~f:(fun json ->
      let fields = to_assoc json in
      if
        List.length fields <> 3
        || List.contains_dup (List.map fields ~f:fst) ~compare:String.compare
      then failwith "expected exactly event_id, account and reason";
      let id =
        match member "event_id" json with
        | `Int n -> Int64.of_int n
        | `Intlit s -> Int64.of_string s
        | _ -> failwith "event_id must be an integer"
      in
      create
        ~event_id:id
        ~account:(member "account" json |> to_string)
        ~reason:(member "reason" json |> to_string)
      |> Or_error.ok_exn))
;;
