open Base

let all =
  Json.decode (Jsont.list Mt940_profile.codec) Mt940_profiles_data.json |> Or_error.ok_exn
;;

let find institution =
  List.find all ~f:(fun (p : Mt940_profile.t) -> String.equal p.institution institution)
;;
