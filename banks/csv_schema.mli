open Base

module Cell : sig
  type 'a t

  val create : ?name:string -> (string -> 'a) -> 'a t
  val validated : ?name:string -> (string -> 'a Or_error.t) -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val string : string t
  val trimmed_string : string t
  val required : ?message:string -> 'a t -> 'a t
  val opt : 'a t -> 'a option t
  val literal : ?message:string -> string -> string t
  val one_of : ?message:string -> string list -> string t
  val text : ?name:string -> unit -> string t
  val token : ?name:string -> unit -> string t
  val currency : ?name:string -> unit -> Iso4217.t t
  val iban : ?name:string -> unit -> Iban.t t
  val iso_date : ?message:string -> unit -> Date.t t
  val dmy_date : ?message:string -> unit -> Date.t t

  val utc_clock
    :  ?layout:[ `Iso_seconds | `Dmy_millis ]
    -> ?message:string
    -> unit
    -> Ptime.t t

  val quantity
    :  ?sign:[ `Any | `Negative_only | `None ]
    -> ?grouping:[ `None | `Spaces ]
    -> ?decimal_mark:[ `Dot | `Comma ]
    -> ?decimals:[ `Optional | `Exact of int ]
    -> ?message:string
    -> unit
    -> Quantity.t t

  val french_amount : Quantity.t t
  val bunq_amount : Quantity.t t
  val decimal_balance : Quantity.t t
end

type 'a raw_row =
  { row_number : int
  ; fields : string list
  ; value : 'a
  }

type ('f, 'res) step
type 'row t

val make : 'f -> ('f, 'f) step
val col : ?pos:int -> ?name:string -> 'a Cell.t -> ('f, 'a -> 'g) step -> ('f, 'g) step
val col_pos : ?name:string -> int -> 'a Cell.t -> ('f, 'a -> 'g) step -> ('f, 'g) step
val col_name : string -> 'a Cell.t -> ('f, 'a -> 'g) step -> ('f, 'g) step
val row_number : ('f, int -> 'g) step -> ('f, 'g) step
val raw_fields : ('f, string list -> 'g) step -> ('f, 'g) step

val create
  :  name:string
  -> separator:char
  -> header:string list
  -> ?trim_header:bool
  -> ?trim_cells:bool
  -> ?skip_blank_rows:bool
  -> ?expected_columns:int
  -> ?columns_message:(int -> int -> string)
  -> ?header_message:string
  -> ?validate_row:('row -> unit)
  -> ?validate_doc:('row list -> unit)
  -> ('f, 'row) step
  -> 'row t

val name : _ t -> string
val expected_header : _ t -> string list
val recognizes : _ t -> string -> bool
val of_fields : 'row t -> row_number:int -> string list -> 'row Or_error.t
val parse : 'row t -> string -> 'row list Or_error.t
val parse_raw : 'row t -> string -> 'row raw_row list Or_error.t
