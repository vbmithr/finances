open Base
open Finances

let paths env =
  App_paths.resolve
    ~home:"/home/example"
    ~getenv:(List.Assoc.find env ~equal:String.equal)
;;

let defaults () =
  let p = paths [] in
  Alcotest.(check string)
    "default data"
    "/home/example/.local/share/fin/fin.sqlite3"
    p.database
;;

let overrides () =
  let p = paths [ "XDG_DATA_HOME", "/data"; "XDG_CONFIG_HOME", "/config" ] in
  Alcotest.(check string) "override data" "/data/fin/fin.sqlite3" p.database
;;

let invalid_overrides () =
  let a = paths []
  and b = paths [ "XDG_DATA_HOME", "relative"; "XDG_CONFIG_HOME", "" ] in
  Alcotest.(check string) "relative data ignored" a.database b.database
;;

let () =
  Alcotest.run
    "XDG paths"
    [ ( "resolution"
      , List.map
          [ "defaults", defaults
          ; "absolute overrides", overrides
          ; "invalid overrides", invalid_overrides
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
