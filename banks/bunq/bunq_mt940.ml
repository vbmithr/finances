open Base

type t =
  { contents : string
  ; account_id : string
  ; currency : string
  ; opening_on : Date.t
  ; closing_on : Date.t
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; transactions : Bunq_transaction.t list
  }

let recognizes s =
  let p = String.prefix s 4096 in
  String.is_prefix p ~prefix:"BUNQ" && String.is_substring p ~substring:":60F:"
;;

let matches pattern s = Re.exec_opt (Re.Perl.compile_pat pattern) s

let date s =
  if String.length s <> 6 then failwith "invalid Bunq YYMMDD date";
  Date.of_string
    ("20"
     ^ String.prefix s 2
     ^ "-"
     ^ String.sub s ~pos:2 ~len:2
     ^ "-"
     ^ String.drop_prefix s 4)
;;

let balance s =
  match matches "^([CD])([0-9]{6})([A-Z]{3})([0-9]+,[0-9]*)$" s with
  | None -> failwith "invalid Bunq booked balance"
  | Some g ->
    let get = Re.Group.get g in
    let amount = Quantity.of_string (String.tr (get 4) ~target:',' ~replacement:'.') in
    date (get 2), get 3, if String.equal (get 1) "D" then Quantity.neg amount else amount
;;

let parse contents =
  Or_error.try_with (fun () ->
    if (not (Stdlib.String.is_valid_utf_8 contents)) || not (recognizes contents)
    then failwith "expected UTF-8 Bunq MT940";
    let lines = String.split_lines contents in
    let fields = ref []
    and current = ref None
    and ended = ref false in
    let finish () =
      Option.iter !current ~f:(fun (tag, parts) ->
        fields
        := (tag, String.concat ~sep:"\n" (List.rev parts) |> String.strip) :: !fields);
      current := None
    in
    List.iter lines ~f:(fun line ->
      if String.equal line "-"
      then (
        finish ();
        ended := true)
      else if String.is_empty (String.strip line)
      then ()
      else if !ended
      then failwith "unexpected content after MT940 terminator"
      else (
        match matches "^:([0-9A-Z]+):(.*)$" line with
        | Some g ->
          finish ();
          current := Some (Re.Group.get g 1, [ Re.Group.get g 2 ])
        | None ->
          (match !current with
           | None when List.is_empty !fields -> ()
           | Some ("86", parts) -> current := Some ("86", line :: parts)
           | _ -> failwith "unsupported Bunq MT940 continuation")));
    finish ();
    if not !ended then failwith "missing Bunq MT940 terminator";
    let fields = List.rev !fields in
    let required tag =
      match
        List.filter_map fields ~f:(fun (k, v) ->
          if String.equal k tag then Some v else None)
      with
      | [ v ] -> v
      | _ -> Printf.failwithf "expected exactly one MT940 %s" tag ()
    in
    ignore (required "20" : string);
    ignore (required "28" : string);
    let account_id, currency =
      match
        String.split (required "25") ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
      with
      | [ a; c ] -> a, c
      | _ -> failwith "expected Bunq account and currency"
    in
    ignore (Validation.token "Bunq account" account_id |> Or_error.ok_exn : string);
    ignore (Validation.token "Bunq currency" currency |> Or_error.ok_exn : string);
    let opening_on, opening_currency, opening = balance (required "60F")
    and closing_on, closing_currency, closing = balance (required "62F") in
    if
      (not
         (String.equal currency opening_currency && String.equal currency closing_currency))
      || Date.(opening_on > closing_on)
    then failwith "inconsistent Bunq statement balance interval or currency";
    let counts = Hashtbl.create (module String) in
    let rec entries acc = function
      | [] -> List.rev acc
      | ("61", raw) :: ("86", memo) :: rest ->
        let g =
          match
            matches "^([0-9]{6})([0-9]{4})([CD])([0-9]+,[0-9]*)(N[A-Z]{3})NONREF$" raw
          with
          | Some g -> g
          | _ -> failwith "unsupported Bunq MT940 entry"
        in
        let get = Re.Group.get g in
        let value_on = date (get 1) in
        let md = get 2 in
        let candidates =
          List.filter_map
            [ Date.year value_on - 1; Date.year value_on; Date.year value_on + 1 ]
            ~f:(fun year ->
              Or_error.try_with (fun () ->
                Date.of_string
                  (Printf.sprintf
                     "%04d-%s-%s"
                     year
                     (String.prefix md 2)
                     (String.drop_prefix md 2)))
              |> Result.ok)
        in
        let booked_on =
          List.min_elt candidates ~compare:(fun a b ->
            Int.compare (Int.abs (Date.diff a value_on)) (Int.abs (Date.diff b value_on)))
          |> Option.value_exn
        in
        if
          Int.abs (Date.diff booked_on value_on) > 183
          || Date.(booked_on < opening_on || booked_on > closing_on)
        then failwith "Bunq entry outside statement interval";
        let name_prefix, description =
          match String.substr_index memo ~pattern:"/REMI/" with
          | None -> failwith "missing Bunq remittance marker"
          | Some i -> String.prefix memo i, String.drop_prefix memo (i + 6)
        in
        let counterparty, name =
          match String.chop_prefix name_prefix ~prefix:"/NAME/" with
          | Some name -> "", name
          | None ->
            (match
               ( String.chop_prefix name_prefix ~prefix:"/IBAN/"
               , String.substr_index name_prefix ~pattern:"/NAME/" )
             with
             | Some _, Some i ->
               ( String.sub name_prefix ~pos:6 ~len:(i - 6)
               , String.drop_prefix name_prefix (i + 6) )
             | _ -> failwith "unsupported Bunq counterparty fields")
        in
        let amount =
          Quantity.of_string (String.tr (get 4) ~target:',' ~replacement:'.')
        in
        let amount = if String.equal (get 3) "D" then Quantity.neg amount else amount in
        let key =
          Bunq_transaction.key
            ~account_id
            ~booked_on
            ~value_on
            ~amount
            ~counterparty
            ~name
            ~description
        in
        Hashtbl.incr counts key;
        let trn =
          Bunq_transaction.create
            ~account_id
            ~currency
            ~booked_on
            ~value_on
            ~amount
            ~counterparty
            ~name
            ~description
            ~occurrence:(Hashtbl.find_exn counts key)
          |> Or_error.ok_exn
        in
        entries (trn :: acc) rest
      | (tag, _) :: rest
        when List.mem [ "20"; "25"; "28"; "60F"; "62F" ] tag ~equal:String.equal ->
        entries acc rest
      | _ -> failwith "unsupported Bunq MT940 field or unpaired transaction"
    in
    let transactions = entries [] fields in
    let movements =
      List.fold transactions ~init:Quantity.zero ~f:(fun total tx ->
        Quantity.(total + tx.Bunq_transaction.amount))
    in
    if not Quantity.(opening + movements = closing)
    then failwith "Bunq opening balance plus movements does not equal closing balance";
    { contents
    ; account_id
    ; currency
    ; opening_on
    ; closing_on
    ; opening
    ; closing
    ; transactions
    })
  |> fun result -> Or_error.tag result ~tag:"Bunq MT940"
;;
