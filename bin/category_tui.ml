open Base
open Finances
open Finances_store
open Or_error.Let_syntax

let reserved = [ 'q'; 's'; 'a'; 'u'; 'b'; 'x' ]

let shortcut_keys =
  String.to_list "abcdefghijklmnopqrstuvwxyz"
  |> List.filter ~f:(fun c -> not (List.mem reserved c ~equal:Char.equal))
;;

let parse_shortcuts categories specifications =
  let parse specification =
    match String.lsplit2 specification ~on:'=' with
    | Some (key, category)
      when String.length key = 1
           && List.mem shortcut_keys key.[0] ~equal:Char.equal
           && List.mem categories category ~equal:String.equal -> Ok (key.[0], category)
    | _ ->
      Or_error.errorf
        "invalid shortcut %s: use an available lowercase letter and an existing category"
        specification
  in
  let%bind shortcuts = List.map specifications ~f:parse |> Or_error.combine_errors in
  if
    List.contains_dup (List.map shortcuts ~f:fst) ~compare:Char.compare
    || List.contains_dup (List.map shortcuts ~f:snd) ~compare:String.compare
  then Or_error.error_string "shortcut letters and categories must be unique"
  else Ok shortcuts
;;

let add_shortcuts shortcuts categories =
  List.fold categories ~init:shortcuts ~f:(fun bindings category ->
    if List.exists bindings ~f:(fun (_, value) -> String.equal value category)
    then bindings
    else (
      let available =
        List.filter shortcut_keys ~f:(fun key ->
          not (List.Assoc.mem bindings key ~equal:Char.equal))
      in
      let last = String.split category ~on:':' |> List.last_exn in
      let preferred =
        String.to_list (String.lowercase last)
        |> List.find ~f:(fun key -> List.mem available key ~equal:Char.equal)
      in
      match Option.first_some preferred (List.hd available) with
      | None -> bindings
      | Some key -> bindings @ [ key, category ]))
;;

type mode =
  | Normal
  | Search of string * int
  | Browse of string * int
  | Add of string

let remove_last_utf8 value =
  if String.is_empty value
  then value
  else (
    let rec start i =
      if i > 0 && Char.to_int value.[i] land 0xc0 = 0x80 then start (i - 1) else i
    in
    String.prefix value (start (String.length value - 1)))
;;

let run db rows categories specifications =
  let%bind configured = parse_shortcuts categories specifications in
  let bindings = ref (add_shortcuts configured categories) in
  let categories = ref categories in
  let new_categories = ref [] in
  let pending = ref rows in
  let skipped = ref 0 in
  let applied = ref [] in
  let match_exact = ref true in
  let mode = ref Normal in
  let message = ref "" in
  let terminal = Notty_unix.Term.create ~mouse:false ~bpaste:false () in
  let exact_matches (row : Store.bank_review) =
    if String.is_empty (String.strip row.description)
    then []
    else List.filter (List.tl_exn !pending) ~f:(Category_similarity.exact_match row)
  in
  let assign category =
    match !pending with
    | [] -> ()
    | (row : Store.bank_review) :: _ ->
      let matches = if !match_exact then exact_matches row else [] in
      let selected = row :: matches in
      let reason = "Classified in category queue" in
      let selected_ids =
        Set.of_list (module Int64) (List.map selected ~f:(fun r -> r.Store.event_id))
      in
      pending
      := List.filter !pending ~f:(fun r -> not (Set.mem selected_ids r.Store.event_id));
      applied := (selected, category, reason) :: !applied;
      message
      := Printf.sprintf "Staged %s (%d transaction(s))" category (List.length selected);
      mode := Normal
  in
  let undo () =
    match !applied with
    | [] -> message := "Nothing to undo."
    | (rows, _, _) :: rest ->
      applied := rest;
      pending := rows @ !pending;
      message := "Last assignment undone."
  in
  let save () =
    let batches = List.rev !applied in
    let expected =
      List.concat_map batches ~f:(fun (rows, _, _) ->
        List.map rows ~f:(fun (row : Store.bank_review) ->
          ( row.event_id
          , Option.map row.reason ~f:(fun reason -> row.counter_account, reason) )))
    in
    let%bind decisions =
      List.concat_map batches ~f:(fun (rows, category, reason) ->
        List.map rows ~f:(fun (row : Store.bank_review) ->
          Bank_category.create ~event_id:row.event_id ~account:category ~reason))
      |> Or_error.combine_errors
    in
    Store.review_bank_categories ~categories:!new_categories db ~expected decisions
  in
  let filtered query =
    List.filter !categories ~f:(fun category ->
      String.Caseless.is_substring category ~substring:query)
  in
  let children prefix =
    let prefix_with_separator = if String.is_empty prefix then "" else prefix ^ ":" in
    List.filter_map !categories ~f:(fun category ->
      Option.bind
        (String.chop_prefix category ~prefix:prefix_with_separator)
        ~f:(fun rest ->
          if String.is_empty rest
          then None
          else Some (prefix_with_separator ^ List.hd_exn (String.split rest ~on:':'))))
    |> List.dedup_and_sort ~compare:String.compare
  in
  let is_category name = List.mem !categories name ~equal:String.equal in
  let has_children name = not (List.is_empty (children name)) in
  let parent name =
    match String.rsplit2 name ~on:':' with
    | None -> ""
    | Some (prefix, _) -> prefix
  in
  let outcome =
    Exn.protect
      ~finally:(fun () -> Notty_unix.Term.release terminal)
      ~f:(fun () ->
        let rec loop () =
          let width, height = Notty_unix.Term.size terminal in
          let width = Int.max 0 width
          and height = Int.max 0 height in
          let line ?(attr = Notty.A.empty) value =
            Notty.I.hsnap width (Notty.I.string attr (Settlement_tui.safe_text value))
          in
          let wrap ?(attr = Notty.A.empty) value =
            if width = 0
            then []
            else (
              let image = Notty.I.string attr (Settlement_tui.safe_text value) in
              let count = Int.max 1 ((Notty.I.width image + width - 1) / width) in
              List.init count ~f:(fun i ->
                let right = Int.max 0 (Notty.I.width image - ((i + 1) * width)) in
                Notty.I.hsnap width (Notty.I.hcrop (i * width) right image)))
          in
          let transaction =
            match !pending with
            | [] ->
              [ line
                  "Queue complete. Undo or quit; skipped transactions remain \
                   unclassified."
              ]
            | (row : Store.bank_review) :: _ ->
              let m = row.transaction in
              wrap
                (Printf.sprintf
                   "#%Ld  %s  %s / %s"
                   row.event_id
                   (Date.to_string m.booked_on)
                   row.institution
                   m.account.account_id)
              @ wrap
                  ~attr:
                    (if
                       Option.value_map
                         (Sys.getenv "NO_COLOR")
                         ~default:false
                         ~f:(fun s -> not (String.is_empty s))
                     then Notty.A.empty
                     else if Quantity.(m.amount > zero)
                     then Notty.A.(fg green)
                     else if Quantity.(m.amount < zero)
                     then Notty.A.(fg red)
                     else Notty.A.empty)
                  (Printf.sprintf
                     "%s %s"
                     (Quantity.to_string m.amount)
                     (Iso4217.to_alpha_code m.account.currency))
              @ wrap row.description
              @ [ line
                    (Printf.sprintf
                       "Exact matches: %d  (x: %s)"
                       (List.length (exact_matches row))
                       (if !match_exact then "on" else "off"))
                ]
          in
          let can_act =
            width >= 40 && height >= 12 && List.length transaction <= height - 11
          in
          let available = Int.max 0 (height - List.length transaction - 8) in
          let rec visible remaining = function
            | [] -> []
            | (key, category) :: rest ->
              let lines = wrap (Printf.sprintf "  %c  %s" key category) in
              if List.length lines > remaining
              then []
              else (key, category) :: visible (remaining - List.length lines) rest
          in
          let visible_bindings = visible available !bindings in
          let image =
            if not can_act
            then Notty.I.vcat [ line "Too small; resize, s:skip, q:save."; line !message ]
            else (
              let header =
                line
                  ~attr:(Notty.A.st Notty.A.bold)
                  (Printf.sprintf
                     "Category queue | %d left | %d staged"
                     (List.length !pending + !skipped)
                     (List.sum
                        (module Int)
                        !applied
                        ~f:(fun (rows, _, _) -> List.length rows)))
              in
              let body, footer =
                match !mode with
                | Normal ->
                  ( List.concat_map visible_bindings ~f:(fun (key, category) ->
                      wrap (Printf.sprintf "  %c  %s" key category))
                  , [ line "Letter: assign  s: skip  /: search"
                    ; line "a:add b:browse u:undo x:match"
                    ; line "q: save & quit   Ctrl-C: discard"
                    ] )
                | Search (query, selected) ->
                  let choices = filtered query in
                  let chosen =
                    Option.value
                      (List.nth choices selected)
                      ~default:"No matching category."
                  in
                  let lines =
                    wrap
                      (Printf.sprintf
                         "%d/%d %s"
                         (if List.is_empty choices then 0 else selected + 1)
                         (List.length choices)
                         chosen)
                  in
                  ( List.take lines available
                  , [ line ("Search: " ^ query); line "Up/Down Enter select Esc back" ] )
                | Browse (prefix, selected) ->
                  let choices = children prefix in
                  let rec render remaining index = function
                    | [] -> []
                    | category :: rest ->
                      let marker = if has_children category then "+" else " " in
                      let lines =
                        wrap
                          ~attr:
                            (if index = selected
                             then Notty.A.st Notty.A.reverse
                             else Notty.A.empty)
                          (Printf.sprintf
                             "%d/%d %s %s"
                             (index + 1)
                             (List.length choices)
                             marker
                             category)
                      in
                      if List.length lines > remaining
                      then List.take lines remaining
                      else lines @ render (remaining - List.length lines) (index + 1) rest
                  in
                  let body =
                    if List.is_empty choices
                    then [ line "No categories at this level." ]
                    else render available selected (List.drop choices selected)
                  in
                  ( body
                  , [ line ("Browse: " ^ if String.is_empty prefix then "root" else prefix)
                    ; line "Up/Down Enter open Space set Left back"
                    ] )
                | Add value ->
                  [], [ line ("New category: " ^ value); line "Enter: add  Esc: back" ]
              in
              Notty.I.vcat
                ([ header ]
                 @ transaction
                 @ [ line ""; line "Categories:" ]
                 @ body
                 @ [ line !message ]
                 @ footer))
          in
          Notty_unix.Term.image terminal (Notty.I.vsnap height image);
          let edit text key =
            match key with
            | `ASCII c when Char.to_int c >= 32 && Char.to_int c < 127 ->
              text ^ String.of_char c
            | `Uchar u ->
              let buffer = Stdlib.Buffer.create 4 in
              Stdlib.Buffer.add_utf_8_uchar buffer u;
              text ^ Stdlib.Buffer.contents buffer
            | `Backspace -> remove_last_utf8 text
            | _ -> text
          in
          match Notty_unix.Term.event terminal with
          | `End | `Key (`ASCII ('c' | 'C'), [ `Ctrl ]) -> None
          | `Key (`ASCII 'q', []) when Poly.equal !mode Normal ->
            (match save () with
             | Ok changed -> Some changed
             | Error error ->
               message := Error.to_string_hum error;
               loop ())
          | `Key (`Escape, _) when not (Poly.equal !mode Normal) ->
            mode := Normal;
            message := "";
            loop ()
          | `Key (`ASCII 'q', _) when not can_act ->
            (match save () with
             | Ok changed -> Some changed
             | Error error ->
               message := Error.to_string_hum error;
               loop ())
          | `Key (`ASCII 's', []) when (not can_act) && Poly.equal !mode Normal ->
            (match !pending with
             | [] -> ()
             | _ :: rest ->
               pending := rest;
               Int.incr skipped);
            loop ()
          | _ when not can_act -> loop ()
          | `Key (key, mods) ->
            (match !mode, key, mods with
             | Normal, `ASCII 's', [] ->
               (match !pending with
                | [] -> ()
                | _ :: rest ->
                  pending := rest;
                  Int.incr skipped)
             | Normal, `ASCII '/', [] when not (List.is_empty !pending) ->
               mode := Search ("", 0)
             | Normal, `ASCII 'b', [] when not (List.is_empty !pending) ->
               mode := Browse ("", 0)
             | Normal, `ASCII 'a', [] -> mode := Add ""
             | Normal, `ASCII 'u', [] -> undo ()
             | Normal, `ASCII 'x', [] -> match_exact := not !match_exact
             | Normal, `ASCII key, [] ->
               Option.iter
                 (List.Assoc.find visible_bindings key ~equal:Char.equal)
                 ~f:assign
             | Search (query, selected), `Arrow `Up, [] ->
               mode := Search (query, Int.max 0 (selected - 1))
             | Search (query, selected), `Arrow `Down, [] ->
               mode
               := Search
                    ( query
                    , Int.min (Int.max 0 (List.length (filtered query) - 1)) (selected + 1)
                    )
             | Search (query, selected), `Enter, [] ->
               Option.iter
                 (List.nth (filtered query) selected)
                 ~f:(fun category ->
                   if
                     List.length
                       (wrap
                          (Printf.sprintf
                             "%d/%d %s"
                             (selected + 1)
                             (List.length (filtered query))
                             category))
                     <= available
                   then assign category
                   else message := "Resize to see the full category before assigning.")
             | Search (query, _), key, [] -> mode := Search (edit query key, 0)
             | Browse (prefix, selected), `Arrow `Up, [] ->
               mode := Browse (prefix, Int.max 0 (selected - 1))
             | Browse (prefix, selected), `Arrow `Down, [] ->
               mode
               := Browse
                    ( prefix
                    , Int.min
                        (Int.max 0 (List.length (children prefix) - 1))
                        (selected + 1) )
             | Browse (prefix, _), (`Arrow `Left | `Backspace), [] ->
               mode := Browse (parent prefix, 0)
             | Browse (prefix, selected), (`Enter | `Arrow `Right | `ASCII ' '), [] ->
               Option.iter
                 (List.nth (children prefix) selected)
                 ~f:(fun category ->
                   let shown =
                     Printf.sprintf
                       "%d/%d %s %s"
                       (selected + 1)
                       (List.length (children prefix))
                       (if has_children category then "+" else " ")
                       category
                   in
                   if List.length (wrap shown) > available
                   then message := "Resize to see the full category before selecting."
                   else if Poly.equal key (`ASCII ' ')
                   then
                     if is_category category
                     then assign category
                     else message := "Choose a category rather than a group."
                   else if has_children category
                   then mode := Browse (category, 0)
                   else if is_category category
                   then assign category)
             | Add value, `Enter, [] ->
               let value = String.strip value in
               (match
                  Bank_category.create
                    ~event_id:1L
                    ~account:value
                    ~reason:"category queue"
                with
                | Error error -> message := Error.to_string_hum error
                | Ok category when String.equal category.account "equity:unclassified" ->
                  message := "Unclassified is not a category."
                | Ok category ->
                  let value = category.account in
                  if not (List.mem !categories value ~equal:String.equal)
                  then new_categories := value :: !new_categories;
                  categories
                  := List.dedup_and_sort (value :: !categories) ~compare:String.compare;
                  bindings := add_shortcuts !bindings [ value ];
                  mode := Normal;
                  message := "Added " ^ value)
             | Add value, key, [] -> mode := Add (edit value key)
             | _ -> ());
            loop ()
          | _ -> loop ()
        in
        loop ())
  in
  (match outcome with
   | None -> Stdio.print_endline "Queue discarded; nothing saved."
   | Some changed -> Stdio.printf "Saved %d transaction(s).\n" changed);
  Ok ()
;;
