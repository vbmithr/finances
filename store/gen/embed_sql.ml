(* Prints the store's SQL files as an OCaml module. Each query or script becomes a value
   named after its file: [transfers/unmatched_transfers.sql] becomes [unmatched_transfers].
   The files of [views/] become one value, [views], in name order, which every connection
   runs when it opens. [queries] lists the single statements and [scripts] the files of
   several, each ending with a semicolon, for a test that checks each one. [schema.sql] is
   generated from the OCaml schema for reading and is not embedded. *)

let () =
  let paths = List.sort compare (List.tl (Array.to_list Sys.argv)) in
  let name path = Filename.remove_extension (Filename.basename path) in
  let read path = In_channel.with_open_bin path In_channel.input_all in
  let in_views path = Filename.basename (Filename.dirname path) = "views" in
  let paths = List.filter (fun path -> Filename.basename path <> "schema.sql") paths in
  let views, others = List.partition in_views paths in
  let embed name text =
    let rec mentions_delimiter i =
      i + 5 <= String.length text
      && (String.sub text i 5 = "|sql}" || mentions_delimiter (i + 1))
    in
    if mentions_delimiter 0 then failwith (name ^ " contains |sql}");
    Printf.printf "let %s = {sql|%s|sql}\n\n" name text
  in
  let names = List.map name others in
  List.iter
    (fun n ->
       if List.length (List.filter (( = ) n) names) > 1
       then failwith ("two SQL files are named " ^ n))
    names;
  embed "views" (String.concat "\n" (List.map read views));
  List.iter (fun path -> embed (name path) (read path)) others;
  let is_script path =
    List.exists
      (fun line -> String.ends_with ~suffix:";" (String.trim line))
      (String.split_on_char '\n' (read path))
  in
  let listed scripts =
    String.concat
      "; "
      (List.filter_map
         (fun path ->
            if is_script path <> scripts
            then None
            else Some (Printf.sprintf "%S, %s" (name path) (name path)))
         others)
  in
  Printf.printf "let queries = [ %s ]\n\n" (listed false);
  Printf.printf "let scripts = [ %s ]\n" (listed true)
;;
