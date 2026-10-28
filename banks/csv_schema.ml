open Base

module Cell = struct
  type 'a t =
    { name : string
    ; decode : string -> 'a
    }

  let create ?(name = "cell") decode = { name; decode }
  let validated ?(name = "cell") f = { name; decode = (fun s -> f s |> Or_error.ok_exn) }
  let map cell ~f = { name = cell.name; decode = (fun s -> f (cell.decode s)) }
  let string = create ~name:"string" Fn.id
  let trimmed_string = create ~name:"trimmed_string" String.strip

  let required ?(message = "CSV cell is required") cell =
    { name = cell.name
    ; decode = (fun s -> if String.is_empty s then failwith message else cell.decode s)
    }
  ;;

  let opt cell =
    { name = cell.name
    ; decode = (fun s -> if String.is_empty s then None else Some (cell.decode s))
    }
  ;;

  let literal ?message expected =
    let msg = Option.value message ~default:(Printf.sprintf "expected %S" expected) in
    create ~name:expected (fun s ->
      if not (String.equal s expected) then failwith msg else s)
  ;;

  let one_of ?message allowed =
    let msg =
      Option.value
        message
        ~default:(Printf.sprintf "expected one of %s" (String.concat ~sep:", " allowed))
    in
    create ~name:"enum" (fun s ->
      if not (List.mem allowed s ~equal:String.equal) then failwith msg else s)
  ;;

  let text ?(name = "CSV text") () = validated ~name (Validation.text name)
  let token ?(name = "CSV token") () = validated ~name (Validation.token name)
  let currency ?(name = "CSV currency") () = validated ~name (Validation.currency name)
  let iban ?(name = "CSV IBAN") () = validated ~name Iban.of_string
  let iso_date_syntax = Re.Perl.compile_pat "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"

  let iso_date ?(message = "expected an ISO calendar date") () =
    create ~name:"iso_date" (fun s ->
      if not (Re.execp iso_date_syntax s) then failwith message;
      Date.of_string s)
  ;;

  let dmy_date_syntax = Re.Perl.compile_pat "^[0-9]{2}-[0-9]{2}-[0-9]{4}$"

  let dmy_date ?(message = "expected a DD-MM-YYYY calendar date") () =
    create ~name:"dmy_date" (fun s ->
      if not (Re.execp dmy_date_syntax s) then failwith message;
      let iso =
        String.sub s ~pos:6 ~len:4
        ^ "-"
        ^ String.sub s ~pos:3 ~len:2
        ^ "-"
        ^ String.prefix s 2
      in
      Date.of_string iso)
  ;;

  let quantity
        ?(sign = `Any)
        ?(grouping = `None)
        ?(decimal_mark = `Dot)
        ?(decimals = `Optional)
        ?(message = "invalid CSV decimal quantity")
        ()
    =
    let sign_pat =
      match sign with
      | `Any -> "[+-]?"
      | `Negative_only -> "-?"
      | `None -> ""
    in
    let int_pat =
      match grouping with
      | `None -> "[0-9]+"
      | `Spaces -> "([0-9]+|[0-9]{1,3}( [0-9]{3})+)"
    in
    let mark_pat =
      match decimal_mark with
      | `Dot -> "\\."
      | `Comma -> ","
    in
    let dec_pat =
      match decimals with
      | `Optional -> "(" ^ mark_pat ^ "[0-9]+)?"
      | `Exact n -> Printf.sprintf "%s[0-9]{%d}" mark_pat n
    in
    let syntax = Re.Perl.compile_pat ("^" ^ sign_pat ^ int_pat ^ dec_pat ^ "$") in
    create ~name:"quantity" (fun raw ->
      let normalized =
        match grouping with
        | `None -> raw
        | `Spaces ->
          String.substr_replace_all raw ~pattern:"\194\160" ~with_:" "
          |> fun s -> String.substr_replace_all s ~pattern:"\226\128\175" ~with_:" "
      in
      if not (Re.execp syntax normalized) then failwith message;
      let cleaned =
        match grouping with
        | `None -> normalized
        | `Spaces -> String.filter normalized ~f:(fun c -> not (Char.equal c ' '))
      in
      let canonical =
        match decimal_mark with
        | `Dot -> cleaned
        | `Comma -> String.tr cleaned ~target:',' ~replacement:'.'
      in
      Quantity.of_string canonical)
  ;;

  let french_amount =
    quantity
      ~sign:`Any
      ~grouping:`Spaces
      ~decimal_mark:`Comma
      ~decimals:`Optional
      ~message:"invalid French movement amount"
      ()
  ;;

  let bunq_amount =
    quantity
      ~sign:`Negative_only
      ~grouping:`Spaces
      ~decimal_mark:`Comma
      ~decimals:(`Exact 2)
      ~message:"invalid Bunq CSV amount"
      ()
  ;;

  let decimal_balance =
    quantity
      ~sign:`Any
      ~grouping:`None
      ~decimal_mark:`Dot
      ~decimals:`Optional
      ~message:"invalid decimal account balance"
      ()
  ;;
end

type 'a raw_row =
  { row_number : int
  ; fields : string list
  ; value : 'a
  }

type row_context =
  { row_number : int
  ; raw_fields : string list
  ; cells : string array
  }

type ('f, 'res) step =
  { compile : trim_header:bool -> header:string list -> row_context -> 'res }

let make f = { compile = (fun ~trim_header:_ ~header:_ _ctx -> f) }

let resolve_column ~trim_header ~header ?pos ?name () =
  let normalize s = if trim_header then String.strip s else s in
  let normalized_header = List.map header ~f:normalize in
  let count = List.length normalized_header in
  match pos, name with
  | Some idx, Some expected ->
    if idx < 0 || idx >= count
    then
      failwith
        (Printf.sprintf
           "column index %d out of bounds for header of %d columns"
           idx
           count);
    let actual = List.nth_exn normalized_header idx in
    if not (String.equal actual (normalize expected))
    then
      failwith
        (Printf.sprintf "expected header %S at column %d, found %S" expected idx actual);
    idx, actual
  | Some idx, None ->
    if idx < 0 || idx >= count
    then
      failwith
        (Printf.sprintf
           "column index %d out of bounds for header of %d columns"
           idx
           count);
    idx, List.nth_exn normalized_header idx
  | None, Some expected ->
    let target = normalize expected in
    let matches =
      List.filter_mapi normalized_header ~f:(fun i h ->
        if String.equal h target then Some i else None)
    in
    (match matches with
     | [ idx ] -> idx, target
     | [] -> failwith (Printf.sprintf "missing column %S in header" expected)
     | _ ->
       failwith
         (Printf.sprintf
            "ambiguous column %S in header: column names must be unique when selected by \
             name"
            expected))
  | None, None -> failwith "column selector must specify pos or name"
;;

let col ?pos ?name (cell : _ Cell.t) prev =
  { compile =
      (fun ~trim_header ~header ->
        let idx, label = resolve_column ~trim_header ~header ?pos ?name () in
        let prev_fn = prev.compile ~trim_header ~header in
        fun ctx ->
          let f = prev_fn ctx in
          let raw_val = ctx.cells.(idx) in
          let value =
            Or_error.try_with (fun () -> cell.decode raw_val)
            |> Or_error.tag ~tag:(Printf.sprintf "column %d (%s)" (idx + 1) label)
            |> Or_error.ok_exn
          in
          f value)
  }
;;

let col_pos ?name pos cell prev = col ~pos ?name cell prev
let col_name name cell prev = col ~name cell prev

let row_number prev =
  { compile =
      (fun ~trim_header ~header ->
        let prev_fn = prev.compile ~trim_header ~header in
        fun ctx -> (prev_fn ctx) ctx.row_number)
  }
;;

let raw_fields prev =
  { compile =
      (fun ~trim_header ~header ->
        let prev_fn = prev.compile ~trim_header ~header in
        fun ctx -> (prev_fn ctx) ctx.raw_fields)
  }
;;

type 'row t =
  { name : string
  ; separator : char
  ; header : string list
  ; trim_header : bool
  ; trim_cells : bool
  ; skip_blank_rows : bool
  ; expected_columns : int
  ; columns_message : int -> int -> string
  ; header_message : string
  ; validate_row : 'row -> unit
  ; validate_doc : 'row list -> unit
  ; decode_ctx : row_context -> 'row
  }

let create
      ~name
      ~separator
      ~header
      ?(trim_header = false)
      ?(trim_cells = false)
      ?(skip_blank_rows = false)
      ?expected_columns
      ?columns_message
      ?header_message
      ?(validate_row = ignore)
      ?(validate_doc = ignore)
      step
  =
  let expected_columns = Option.value expected_columns ~default:(List.length header) in
  let columns_message =
    Option.value columns_message ~default:(fun expected _actual ->
      Printf.sprintf "expected %d CSV columns" expected)
  in
  let header_message =
    Option.value header_message ~default:(Printf.sprintf "unsupported %s CSV header" name)
  in
  let decode_ctx = step.compile ~trim_header ~header in
  { name
  ; separator
  ; header
  ; trim_header
  ; trim_cells
  ; skip_blank_rows
  ; expected_columns
  ; columns_message
  ; header_message
  ; validate_row
  ; validate_doc
  ; decode_ctx
  }
;;

let name t = t.name
let expected_header t = t.header

let recognizes t contents =
  Csv_reader.recognizes
    ~separator:t.separator
    ~trim:t.trim_header
    ~header:t.header
    contents
;;

let of_fields t ~row_number fields =
  Or_error.try_with (fun () ->
    if row_number <= 0 then failwith "CSV row number must be positive";
    let actual = List.length fields in
    if actual <> t.expected_columns
    then failwith (t.columns_message t.expected_columns actual);
    let cells =
      if t.trim_cells
      then Array.of_list (List.map fields ~f:String.strip)
      else Array.of_list fields
    in
    let row = t.decode_ctx { row_number; raw_fields = fields; cells } in
    t.validate_row row;
    row)
  |> Or_error.tag ~tag:(Printf.sprintf "CSV row %d" row_number)
;;

let parse_raw t contents =
  Or_error.try_with (fun () ->
    Csv_reader.check_utf8 ~name:t.name contents;
    let input = Csv_reader.channel ~separator:t.separator contents in
    Csv_reader.check_header
      ~trim:t.trim_header
      ~header:t.header
      ~message:t.header_message
      input;
    let raw_rows =
      Csv.input_all input
      |> List.mapi ~f:(fun index fields ->
        if
          t.skip_blank_rows
          && List.for_all fields ~f:(fun s -> String.is_empty (String.strip s))
        then None
        else (
          let row_number = index + 1 in
          let value = of_fields t ~row_number fields |> Or_error.ok_exn in
          Some { row_number; fields; value }))
      |> List.filter_opt
    in
    t.validate_doc (List.map raw_rows ~f:(fun r -> r.value));
    raw_rows)
  |> Or_error.tag ~tag:(t.name ^ " CSV")
;;

let parse t contents =
  Or_error.map (parse_raw t contents) ~f:(List.map ~f:(fun r -> r.value))
;;
