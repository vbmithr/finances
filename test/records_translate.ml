open Base
open Stdio
open Finances
open Finances_store

(* Moving an institution to records gives its rows a new identity, so decisions exported before the
   move name rows that no longer exist. This reads statements through both the old adapter and
   records, pairs their rows in source order, checks the pairs by date and amount, and rewrites a
   decisions export. Each decision's payload hash also says the old row is the one it was made on.
   It goes when the old adapters go.

   records_translate EXPORT OUTPUT [INSTITUTION=PATH...]

   Also translates transfer and exchange exports, which hold the same decisions, and account
   snapshots, where only Revolut's account names changed. *)

let ok = Or_error.ok_exn
let sha256 text = Digestif.SHA256.(to_hex (digest_string text))

let old_adapter institution : (module Adapter.S) option =
  match Builtin.definition institution with
  | Some definition -> Some (Institution_definition.adapter definition)
  | None ->
    Option.map (Builtin.movement_profile institution) ~f:(fun profile ->
      Movement_csv_adapter.make profile ())
;;

let rec files path =
  if Stdlib.Sys.is_directory path
  then
    Stdlib.Sys.readdir path
    |> Array.to_list
    |> List.sort ~compare:String.compare
    |> List.filter ~f:(fun name -> not (String.is_prefix name ~prefix:"."))
    |> List.concat_map ~f:(fun name -> files (Stdlib.Filename.concat path name))
  else (
    let contents = In_channel.read_all path in
    if Finances_import.Zip_archive.is_zip path
    then
      ok (Finances_import.Zip_archive.files ~max_bytes:536870912L contents)
      |> List.map ~f:(fun (member, contents) -> path ^ "/" ^ member, contents)
    else [ path, contents ])
;;

let events (module A : Adapter.S) ~label ~filename contents =
  match A.probe ~filename contents with
  | None -> []
  | Some format ->
    let context : Adapter.context =
      { account_label = Option.some_if A.takes_account_label label }
    in
    let document = ok (A.parse ~context ~format contents) in
    List.concat_map document.statements ~f:(fun s -> s.events) @ document.events
;;

type problems =
  { mutable unequal_rows : int
  ; mutable unequal_files : int
  }

(* What a row states, to check a pair: its date and amount, when it is a bank movement. *)
let stated (module A : Adapter.S) (event : Source_event.t) =
  match A.decode event with
  | Ok movement ->
    Some (Date.to_string movement.booked_on, Quantity.to_string movement.amount)
  | Error _ -> None
;;

let pair_file ~problems ~mapping ~old_adapter ~new_adapter ~label (filename, contents) =
  let old_events = events old_adapter ~label ~filename contents
  and new_events = events new_adapter ~label ~filename contents in
  (* Reverted rows, which the old adapters drop, have no movements. *)
  let new_events =
    if List.length old_events = List.length new_events
    then new_events
    else
      List.filter new_events ~f:(fun (e : Source_event.t) ->
        match Records.row_of_string e.payload with
        | Ok (_, record) -> not (List.is_empty record.movements)
        | Error _ -> true)
  in
  match List.zip old_events new_events with
  | Unequal_lengths -> problems.unequal_files <- problems.unequal_files + 1
  | Ok pairs ->
    List.iter pairs ~f:(fun ((o : Source_event.t), (n : Source_event.t)) ->
      (match stated old_adapter o, stated new_adapter n with
       | Some a, Some b when not ([%equal: string * string] a b) ->
         problems.unequal_rows <- problems.unequal_rows + 1
       | _ -> ());
      Hashtbl.set
        mapping
        ~key:(o.venue, o.account, o.kind, o.source_id)
        ~data:(n, sha256 o.payload))
;;

let revolut_account account_id institution =
  if String.equal institution "revolut" then "personal" else account_id
;;

(* Only Revolut's account names changed: its label became one fixed ID. *)
let translate_accounts rows =
  let reference (r : Bank_account_config.reference) =
    { r with account_id = revolut_account r.account_id r.institution }
  in
  List.map rows ~f:(fun (row : Bank_account_config.t) ->
    { row with
      reference = reference row.reference
    ; settlement_account =
        (match row.settlement_account with
         | Set target -> Set (reference target)
         | Keep -> Keep
         | Clear -> Clear)
    })
;;

let () =
  match Array.to_list (Sys.get_argv ()) with
  | _ :: export :: output :: sources ->
    let text = In_channel.read_all export in
    let write codec value =
      Out_channel.write_all output ~data:(Json.encode ~format:Jsont.Indent codec value)
    in
    (match Json.decode Bank_account_config.snapshot_jsont text with
     | Ok rows ->
       write Bank_account_config.snapshot_jsont (translate_accounts rows);
       printf "accounts=%d\n" (List.length rows)
     | Error _ ->
       let mapping = Hashtbl.Poly.create () in
       let problems = { unequal_rows = 0; unequal_files = 0 } in
       List.iter sources ~f:(fun source ->
         let institution, path = String.lsplit2_exn source ~on:'=' in
         let old_adapter = Option.value_exn (old_adapter institution) in
         let new_adapter =
           Option.value_exn (Registry.find Builtin.registry institution)
         in
         List.iter
           (files path)
           ~f:(pair_file ~problems ~mapping ~old_adapter ~new_adapter ~label:"main"));
       let translated = ref 0
       and unknown = ref 0
       and changed_since = ref 0 in
       let translate (d : Category_portable.decision) =
         match
           Hashtbl.find mapping (d.institution, d.source_account, d.kind, d.source_id)
         with
         | None ->
           Int.incr unknown;
           d
         | Some ((n : Source_event.t), old_payload) ->
           Int.incr translated;
           if not (String.equal old_payload d.payload_sha256) then Int.incr changed_since;
           { d with
             source_account = n.account
           ; kind = n.kind
           ; source_id = n.source_id
           ; occurred = Temporal.to_string n.occurred
           ; payload_sha256 = sha256 n.payload
           ; fields_sha256 = Restore.category_fields_hash n
           }
       in
       (match
          ( Json.decode Transfer_portable.codec text
          , Json.decode Exchange_portable.codec text )
        with
        | Ok snapshot, _ ->
          write
            Transfer_portable.codec
            { links =
                List.map snapshot.links ~f:(fun link ->
                  { link with
                    first = translate link.first
                  ; second = translate link.second
                  })
            }
        | _, Ok snapshot ->
          write
            Exchange_portable.codec
            { exchanges =
                List.map snapshot.exchanges ~f:(fun e ->
                  { e with source = translate e.source })
            }
        | _ ->
          let snapshot =
            List.find_map
              Category_portable.
                [ decisions_codec
                ; version4_decisions_codec
                ; previous_decisions_codec
                ; legacy_decisions_codec
                ]
              ~f:(fun codec -> Result.ok (Json.decode codec text))
            |> fun found ->
            Option.value_exn found ~message:"not an export this tool knows"
          in
          write
            Category_portable.decisions_codec
            { snapshot with
              decisions = List.map snapshot.decisions ~f:translate
            ; resolutions =
                List.map snapshot.resolutions ~f:(fun r ->
                  { r with source = translate r.source })
            });
       printf
         "paired rows=%d translated=%d unknown=%d old-payload-differs=%d unequal-rows=%d \
          unequal-files=%d\n"
         (Hashtbl.length mapping)
         !translated
         !unknown
         !changed_since
         problems.unequal_rows
         problems.unequal_files)
  | _ -> prerr_endline "records_translate EXPORT OUTPUT [INSTITUTION=PATH...]"
;;
