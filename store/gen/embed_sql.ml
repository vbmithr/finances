(* Prints one OCaml value per SQL file, named after the file: [views.sql] becomes
   [let views = {sql|...|sql}]. [queries] lists the single statements, for a test that
   compiles each one. *)

let () =
  let paths = List.tl (Array.to_list Sys.argv) in
  let name path = Filename.remove_extension (Filename.basename path) in
  List.iter
    (fun path ->
       let text = In_channel.with_open_bin path In_channel.input_all in
       let rec mentions_delimiter i =
         i + 5 <= String.length text
         && (String.sub text i 5 = "|sql}" || mentions_delimiter (i + 1))
       in
       if mentions_delimiter 0 then failwith (path ^ " contains |sql}");
       Printf.printf "let %s = {sql|%s|sql}\n\n" (name path) text)
    paths;
  Printf.printf
    "let queries = [ %s ]\n"
    (String.concat
       "; "
       (List.filter_map
          (fun path ->
             let name = name path in
             if name = "views" then None else Some (Printf.sprintf "%S, %s" name name))
          paths))
;;
