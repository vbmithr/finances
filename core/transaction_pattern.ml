open Base

type t = Re.re

let compile ~case_sensitive pattern =
  if String.is_empty (String.strip pattern)
  then Or_error.error_string "regular expression must not be empty"
  else
    Or_error.try_with (fun () ->
      Re.Perl.compile_pat ~opts:(if case_sensitive then [] else [ `Caseless ]) pattern)
    |> Or_error.tag
         ~tag:
           "invalid or unsupported regular expression; regex metacharacters such as * \
            must be escaped to match literally (for example \\*{8} matches eight \
            asterisks)"
;;

let matches pattern ~display_name ~descriptions =
  List.exists (Option.to_list display_name @ descriptions) ~f:(Re.execp pattern)
;;
