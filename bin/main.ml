open Base
open Cmdliner

let command_argv () =
  let argv = Stdlib.Array.copy Stdlib.Sys.argv in
  if Stdlib.Array.length argv > 1
  then
    argv.(1)
    <- (match argv.(1) with
        | "op" -> "operation"
        | "acc" -> "account"
        | "inst" -> "institution"
        | "cat" -> "category"
        | command -> command);
  if
    Stdlib.Array.length argv > 2
    && String.equal argv.(1) "operation"
    && String.equal argv.(2) "cat"
  then argv.(2) <- "category";
  if Stdlib.Array.length argv > 1 && String.equal argv.(1) "ledger"
  then (
    let args = Array.to_list argv in
    let rec split prefix = function
      | (("--database" | "-d") as option) :: value :: rest ->
        split (value :: option :: prefix) rest
      | option :: rest when String.is_prefix option ~prefix:"--database=" ->
        split (option :: prefix) rest
      | [ option ]
        when String.equal option "-h" || String.is_prefix option ~prefix:"--help" ->
        List.rev (option :: prefix)
      | "--" :: rest -> List.rev prefix @ ("--" :: rest)
      | rest -> List.rev prefix @ ("--" :: rest)
    in
    match args with
    | program :: name :: rest -> Array.of_list (program :: name :: split [] rest)
    | _ -> argv)
  else argv
;;

let () =
  Stdlib.exit
    (Cmd.eval
       ~argv:(command_argv ())
       (Cmd.group
          ~default:Term.(const Setup_cmd.welcome $ const ())
          (Cmd.info
             "fin"
             ~version:"0.1.0"
             ~doc:"Personal finance accounting and position tracking."
             ~man:
               [ `S Manpage.s_description
               ; `P
                   "Import bank statements into a local database, review accounts and \
                    transactions, and export an hledger journal. Run $(b,fin) without \
                    arguments for a short welcome."
               ; `P
                   "Start with $(b,fin init): it creates the database and shows how to \
                    import statements. Imports and review commands need that database \
                    and say so when it is missing. Accounts are discovered from imported \
                    records."
               ; `S Manpage.s_examples
               ; `Pre
                   "fin init\n\
                    fin import boursobank ~/Statements/boursobank\n\
                    fin status\n\
                    fin account list\n\
                    fin operation list\n\
                    fin operation review\n\
                    fin journal export > my-finances.journal"
               ; `P
                   "Use $(b,fin paths) to inspect storage locations, or append \
                    $(b,--help) to any subcommand for its options."
               ; `P
                   "Command shortcuts: $(b,op) means $(b,operation); $(b,acc) means \
                    $(b,account); $(b,inst) means $(b,institution); $(b,cat) means \
                    $(b,category)."
               ])
          [ Setup_cmd.setup_cmd
          ; Status_cmd.cmd
          ; Setup_cmd.paths_cmd
          ; Journal_cmd.ledger_cmd
          ; Setup_cmd.backup_cmd
          ; Setup_cmd.restore_cmd
          ; Setup_cmd.delete_cmd
          ; Decisions_cmd.decisions_cmd
          ; Account_cmd.cmd
          ; Import_cmd.cmd
          ; Records_cmd.convert_cmd
          ; Category_cmd.cmd
          ; Institution_cmd.cmd
          ; Records_cmd.cmd
          ; Settlement_cmd.cmd
          ; Reversal_cmd.cmd
          ; Rule_cmd.cmd
          ; Db_cmd.cmd
          ; Statement_cmd.cmd
          ; Transfer_cmd.cmd
          ; Operation_cmd.cmd
          ; Journal_cmd.cmd
          ]))
;;
