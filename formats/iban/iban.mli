open Base

(** International Bank Account Numbers, ISO 13616-1:2020.

    An IBAN is a two-letter country code, two check digits and a country-specific
    basic bank account number (BBAN). Check digits follow ISO/IEC 7064 MOD 97-10.
    Each country's BBAN length and structure come from the IBAN Registry kept by
    SWIFT as registration authority (ISO 13616-2); see {!Registry.release}.

    A value of type [t] has passed every check, so it can be compared and stored
    without revalidation. Error messages never include the account number. *)
type t

(** Accepts the electronic or the print format. Whitespace is removed and
    letters are upper-cased; the result must then have a registered country,
    that country's length and BBAN structure, check digits between 02 and 98,
    and a valid MOD 97-10 checksum. *)
val of_string : string -> t Or_error.t

(** Builds an IBAN from a registered country and a BBAN, computing the check
    digits. The BBAN must match the country's registered structure. *)
val create : country:string -> bban:string -> t Or_error.t

(** Electronic format: upper case, no spaces. *)
val to_string : t -> string

(** Print format: groups of four characters separated by single spaces. *)
val to_print : t -> string

val country : t -> string
val check_digits : t -> string
val bban : t -> string

(** Present when the registry defines the identifier's position for the
    country. *)
val bank_identifier : t -> string option

val branch_identifier : t -> string option

(** Whether the registry lists the IBAN's country as a SEPA country. *)
val is_sepa : t -> bool

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val sexp_of_t : t -> Sexp.t

module Registry : sig
  type char_class =
    | Digit (** [n] *)
    | Upper (** [a] *)
    | Alphanumeric (** [c] *)

  type entry = private
    { country : string
    ; name : string
    ; bban : (int * char_class) list
      (** Fixed-length segments, in order, as in the registry notation. *)
    ; length : int (** Full IBAN length. *)
    ; bank_identifier : (int * int) option
      (** Zero-based offset and length within the BBAN. *)
    ; branch_identifier : (int * int) option
    ; sepa : bool
    ; example : string (** The registry's electronic format example. *)
    }

  (** The registry release the table was generated from. *)
  val release : string

  (** Sorted by country code. *)
  val entries : entry list

  val find : string -> entry option

  (** The registry notation, e.g. [4!n4!n12!c]. *)
  val structure : entry -> string
end
