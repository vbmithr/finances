open Base

let has_digit word = String.exists word ~f:Char.is_digit

let words description =
  String.split_on_chars (String.strip description) ~on:[ ' '; '\t'; '\n'; '\r' ]
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let key description =
  words description
  |> List.take_while ~f:(Fn.non has_digit)
  |> fun leading -> List.take leading 2 |> String.concat ~sep:" " |> String.lowercase
;;

let escape word =
  String.concat_map word ~f:(fun c ->
    if String.contains "\\.^$|?*+()[]{}" c
    then Printf.sprintf "\\%c" c
    else String.of_char c)
;;

let pattern key =
  match words key with
  | [] -> ""
  | words -> "^" ^ String.concat ~sep:"\\s+" (List.map words ~f:escape) ^ "(?:\\s|$)"
;;
