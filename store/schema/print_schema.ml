let () =
  Stdio.print_string
    "-- Generated from store/db_schema.ml by store/schema: do not edit. The tables are\n\
     -- defined in OCaml; dune runtest checks this copy and dune promote refreshes it.\n\n";
  Stdio.print_endline Finances_store.Setup.schema_sql
;;
