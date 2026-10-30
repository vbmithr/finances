open Base

type choice =
  { title : string
  ; details : string list
  }

let safe_text value =
  let buffer = Stdlib.Buffer.create (String.length value) in
  Uutf.String.fold_utf_8
    (fun () _ character ->
       match character with
       | `Malformed _ -> Stdlib.Buffer.add_utf_8_uchar buffer (Stdlib.Uchar.of_int 0xfffd)
       | `Uchar scalar ->
         let code = Stdlib.Uchar.to_int scalar in
         if code < 0x20 || (code >= 0x7f && code <= 0x9f)
         then Stdlib.Buffer.add_char buffer ' '
         else Stdlib.Buffer.add_utf_8_uchar buffer scalar)
    ()
    value;
  Stdlib.Buffer.contents buffer
;;

let run choices =
  let choices = Array.of_list choices in
  if Array.is_empty choices
  then None
  else (
    let terminal = Notty_unix.Term.create ~mouse:false ~bpaste:false () in
    Exn.protect
      ~finally:(fun () -> Notty_unix.Term.release terminal)
      ~f:(fun () ->
        let selected = ref 0 in
        let confirming = ref false in
        let rec loop () =
          let width, height = Notty_unix.Term.size terminal in
          let width = Int.max 0 width
          and height = Int.max 0 height in
          let line ?(attr = Notty.A.empty) value =
            Notty.I.hsnap width (Notty.I.string attr (safe_text value))
          in
          let wrap value =
            if width = 0
            then []
            else (
              let image = Notty.I.string Notty.A.empty (safe_text value) in
              let size = Notty.I.width image in
              List.init
                (Int.max 1 ((size + width - 1) / width))
                ~f:(fun index ->
                  Notty.I.hsnap width (Notty.I.hcrop (index * width) 0 image)))
          in
          let detail_images = List.concat_map choices.(!selected).details ~f:wrap in
          let can_confirm =
            width >= 30 && height >= 12 && List.length detail_images <= height - 7
          in
          let image =
            if width < 30 || height < 12
            then Notty.I.vcat [ line "Terminal too small; resize or press q." ]
            else (
              let max_details = Int.max 0 (height - 7) in
              let shown_details = List.take detail_images max_details in
              let list_height = Int.max 1 (height - List.length shown_details - 5) in
              let start = Int.max 0 (!selected - list_height + 1) in
              let list =
                Array.to_list choices
                |> fun xs ->
                List.drop xs start
                |> fun xs ->
                List.take xs list_height
                |> List.mapi ~f:(fun i c ->
                  line
                    ~attr:
                      (if i + start = !selected
                       then Notty.A.st Notty.A.reverse
                       else Notty.A.empty)
                    c.title)
              in
              let header =
                line ~attr:(Notty.A.st Notty.A.bold) "Card settlement review"
              in
              let separator = line "Proposed changes:" in
              let footer =
                if not can_confirm
                then line "Resize to see all details; q: quit"
                else if !confirming
                then line "Confirm this one settlement? y: apply  n/Esc: back  q: quit"
                else line "Up/down: select  Enter: review  q: quit"
              in
              Notty.I.vcat ([ header ] @ list @ [ separator ] @ shown_details @ [ footer ]))
          in
          Notty_unix.Term.image terminal (Notty.I.vsnap height image);
          match Notty_unix.Term.event terminal with
          | `Key (`ASCII ('c' | 'C'), mods) when List.mem mods `Ctrl ~equal:Poly.equal ->
            None
          | `Key (`ASCII 'q', _) | `End -> None
          | `Key (`Escape, _) when !confirming ->
            confirming := false;
            loop ()
          | `Key (`Escape, _) -> None
          | `Key ((`Arrow `Up | `ASCII 'k'), _) when not !confirming ->
            selected := Int.max 0 (!selected - 1);
            loop ()
          | `Key ((`Arrow `Down | `ASCII 'j'), _) when not !confirming ->
            selected := Int.min (Array.length choices - 1) (!selected + 1);
            loop ()
          | `Key (`Enter, _) when (not !confirming) && can_confirm ->
            confirming := true;
            loop ()
          | `Key (`ASCII ('y' | 'Y'), _) when !confirming && can_confirm -> Some !selected
          | `Key (`ASCII ('n' | 'N'), _) when !confirming ->
            confirming := false;
            loop ()
          | _ -> loop ()
        in
        loop ()))
;;
