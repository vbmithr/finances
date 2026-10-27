open Base

module Registry = struct
  type char_class =
    | Digit
    | Upper
    | Alphanumeric

  type entry =
    { country : string
    ; name : string
    ; bban : (int * char_class) list
    ; length : int
    ; bank_identifier : (int * int) option
    ; branch_identifier : (int * int) option
    ; sepa : bool
    ; example : string
    }

  let release = Registry_data.release

  let parse_structure text =
    let rec segments acc i =
      if i = String.length text
      then List.rev acc
      else (
        let j = ref i in
        while !j < String.length text && Char.is_digit text.[!j] do
          Int.incr j
        done;
        if !j = i || !j + 1 >= String.length text || not (Char.equal text.[!j] '!')
        then failwith ("unsupported BBAN structure " ^ text);
        let count = Int.of_string (String.sub text ~pos:i ~len:(!j - i)) in
        let char_class =
          match text.[!j + 1] with
          | 'n' -> Digit
          | 'a' -> Upper
          | 'c' -> Alphanumeric
          | _ -> failwith ("unsupported BBAN structure " ^ text)
        in
        segments ((count, char_class) :: acc) (!j + 2))
    in
    segments [] 0
  ;;

  let structure entry =
    List.map entry.bban ~f:(fun (count, char_class) ->
      Int.to_string count
      ^ "!"
      ^
      match char_class with
      | Digit -> "n"
      | Upper -> "a"
      | Alphanumeric -> "c")
    |> String.concat
  ;;

  (* The registry gives 1-based inclusive positions within the BBAN. *)
  let span = Option.map ~f:(fun (first, last) -> first - 1, last - first + 1)

  let entries =
    List.map
      Registry_data.entries
      ~f:(fun (country, name, structure, bank, branch, sepa, example) ->
        let bban = parse_structure structure in
        { country
        ; name
        ; bban
        ; length = 4 + List.sum (module Int) bban ~f:fst
        ; bank_identifier = span bank
        ; branch_identifier = span branch
        ; sepa
        ; example
        })
  ;;

  let table =
    Hashtbl.of_alist_exn
      (module String)
      (List.map entries ~f:(fun entry -> entry.country, entry))
  ;;

  let find country = Hashtbl.find table country
end

type t = string

let equal = String.equal
let compare = String.compare
let hash = String.hash
let sexp_of_t = String.sexp_of_t
let to_string t = t
let country t = String.prefix t 2
let check_digits t = String.sub t ~pos:2 ~len:2
let bban t = String.drop_prefix t 4

let to_print t =
  List.init
    ((String.length t + 3) / 4)
    ~f:(fun i -> String.sub t ~pos:(i * 4) ~len:(Int.min 4 (String.length t - (i * 4))))
  |> String.concat ~sep:" "
;;

let entry t = Registry.find (country t) |> Option.value_exn
let is_sepa t = (entry t).sepa

let identifier t span =
  Option.map span ~f:(fun (pos, len) -> String.sub (bban t) ~pos ~len)
;;

let bank_identifier t = identifier t (entry t).bank_identifier
let branch_identifier t = identifier t (entry t).branch_identifier

(* ISO/IEC 7064 MOD 97-10 over the rearranged IBAN: BBAN, then country code and
   check digits, with letters expanded to 10..35. *)
let remainder text =
  let rotated = String.drop_prefix text 4 ^ String.prefix text 4 in
  String.fold rotated ~init:0 ~f:(fun remainder c ->
    if Char.is_digit c
    then ((remainder * 10) + Char.get_digit_exn c) % 97
    else ((remainder * 100) + Char.to_int c - Char.to_int 'A' + 10) % 97)
;;

let matches (entry : Registry.entry) bban =
  String.length bban = entry.length - 4
  &&
  let rec check pos = function
    | [] -> true
    | (count, (char_class : Registry.char_class)) :: rest ->
      let ok =
        match char_class with
        | Digit -> Char.is_digit
        | Upper -> Char.is_uppercase
        | Alphanumeric -> Char.is_alphanum
      in
      String.for_all (String.sub bban ~pos ~len:count) ~f:ok && check (pos + count) rest
  in
  check 0 entry.bban
;;

let registered country =
  match Registry.find country with
  | Some entry -> Ok entry
  | None -> Or_error.errorf "IBAN country code %s is not in the IBAN Registry" country
;;

let normalize value =
  String.filter value ~f:(Fn.non Char.is_whitespace) |> String.uppercase
;;

let of_string value =
  let open Or_error.Let_syntax in
  let value = normalize value in
  let%bind () =
    if String.length value < 4 || not (String.for_all value ~f:Char.is_alphanum)
    then Or_error.error_string "IBAN must contain only letters and digits"
    else if
      not
        (Char.is_uppercase value.[0]
         && Char.is_uppercase value.[1]
         && Char.is_digit value.[2]
         && Char.is_digit value.[3])
    then
      Or_error.error_string
        "IBAN must start with a two-letter country code and two check digits"
    else Ok ()
  in
  let country = String.prefix value 2 in
  let%bind entry = registered country in
  let%bind () =
    if String.length value <> entry.length
    then
      Or_error.errorf
        "%s IBAN must have %d characters, not %d"
        country
        entry.length
        (String.length value)
    else if not (matches entry (String.drop_prefix value 4))
    then
      Or_error.errorf
        "%s BBAN does not match the registered structure %s"
        country
        (Registry.structure entry)
    else Ok ()
  in
  let digits = Int.of_string (String.sub value ~pos:2 ~len:2) in
  if digits < 2 || digits > 98
  then Or_error.error_string "IBAN check digits must be between 02 and 98"
  else if remainder value <> 1
  then Or_error.error_string "IBAN checksum is invalid"
  else Ok value
;;

let create ~country ~bban =
  let open Or_error.Let_syntax in
  let country = String.uppercase country
  and bban = normalize bban in
  let%bind entry = registered country in
  if not (matches entry bban)
  then
    Or_error.errorf
      "%s BBAN does not match the registered structure %s"
      country
      (Registry.structure entry)
  else (
    let digits = 98 - remainder (country ^ "00" ^ bban) in
    of_string (Printf.sprintf "%s%02d%s" country digits bban))
;;
