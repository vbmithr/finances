(* Prints one OCaml value per SQL file, named after the file: [views.sql] becomes
   [let views = {sql|...|sql}]. [queries] lists the single statements and [scripts] the
   files of several, ending each with a semicolon, for a test that checks each one.
   [views] runs when a connection opens and is in neither. *)

let () =
  let paths = List.tl (Array.to_list Sys.argv) in
  let name path = Filename.remove_extension (Filename.basename path) in
  let read path = In_channel.with_open_bin path In_channel.input_all in
  let is_script path =
    List.exists
      (fun line -> String.ends_with ~suffix:";" (String.trim line))
      (String.split_on_char '\n' (read path))
  in
  List.iter
    (fun path ->
       let text = read path in
       let rec mentions_delimiter i =
         i + 5 <= String.length text
         && (String.sub text i 5 = "|sql}" || mentions_delimiter (i + 1))
       in
       if mentions_delimiter 0 then failwith (path ^ " contains |sql}");
       Printf.printf "let %s = {sql|%s|sql}\n\n" (name path) text)
    paths;
  let listed scripts =
    String.concat
      "; "
      (List.filter_map
         (fun path ->
            let name = name path in
            if name = "views" || is_script path <> scripts
            then None
            else Some (Printf.sprintf "%S, %s" name name))
         paths)
  in
  Printf.printf "let queries = [ %s ]\n\n" (listed false);
  Printf.printf "let scripts = [ %s ]\n" (listed true)
;;
