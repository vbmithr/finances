open Base
open Lua_runtime

(* Experimental: the caller owns the runtime for a single source file. *)
let create source =
  Or_error.try_with (fun () ->
    let state = load source in
    let interpret =
      match I.getglobal state (L.String "interpret_camt") with
      | L.Function (_, fn) -> fn
      | _ -> failwith "script must define interpret_camt"
    in
    let optional f = function
      | None -> L.Nil
      | Some value -> L.String (f value)
    in
    fun (entry : Camt_source.parsed_entry) (statement : Camt_source.statement_context) ->
      let input =
        table
          [ "proprietary_code", optional Fn.id entry.proprietary_code
          ; "account_servicer_reference", optional Fn.id entry.account_servicer_reference
          ; "description", L.String entry.description
          ; "booked_at", L.String entry.booked_at
          ; "value_on", optional Date.to_string entry.value_on
          ; "amount", L.String (Quantity.to_string entry.amount)
          ; "currency", L.String (Iso4217.to_alpha_code entry.currency)
          ; "direction", L.String (if entry.credit then "credit" else "debit")
          ]
      in
      let context =
        table
          [ "id", L.String statement.id
          ; "account", L.String statement.account.account_id
          ; "account_type", L.String statement.account.account_type
          ; "currency", L.String (Iso4217.to_alpha_code statement.account.currency)
          ; "period_start", L.String statement.period_start
          ; "period_end", L.String statement.period_end
          ]
      in
      match interpret [ input; context ] with
      | [ L.Table result ] ->
        V.Luahash.iter
          (fun key _ ->
             match key with
             | L.String ("reference" | "group" | "supporting") -> ()
             | L.String name -> fail "unknown CAMT interpretation field: %s" name
             | _ -> failwith "CAMT interpretation keys must be strings")
          result;
        let reference = get_string result "reference" in
        let supporting =
          match get result "supporting" with
          | L.Nil | L.Userdata (Userdata.Boolean false) -> false
          | L.Userdata (Userdata.Boolean true) -> true
          | _ -> failwith "supporting must be yes or no"
        in
        let group =
          match get result "group" with
          | L.Nil when not supporting -> None
          | L.String key when not (String.is_empty key) -> Some (key, supporting)
          | _ -> failwith "group must be nonempty; supporting requires a group"
        in
        ({ reference; group } : Camt_source.interpretation)
      | _ -> failwith "interpret_camt must return a table")
;;
