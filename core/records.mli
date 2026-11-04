open Base

(** A records file: what one institution's statements state, as a converter
    prints it. It is evidence, not accounting. See docs/records-format.md. Each line
    decodes exactly when formats/records/fin-records.schema.json accepts it; {!of_string}
    then refuses a file whose lines are well formed but make no sense together. *)

(** How record IDs relate to the source. [Inventory] means the source has no
    identifiers, so identical rows cannot be told apart. *)
type identity =
  | Source
  | Inventory

type converter =
  { name : string
  ; version : string option
  }

type source =
  { name : string
  ; sha256 : string
  }

(** [Unstated] leaves the IBAN to a registered one; [No_iban] says the account
    has none, such as a card. *)
type account_iban =
  | Unstated
  | Iban of string
  | No_iban

type account =
  { id : string
  ; bank : string option
  ; number : string option
  ; account_type : string option
  ; currency : Iso4217.t option
  ; iban : account_iban
  }

(** A date or instant with the text that stated it. {!Temporal.t} drops an instant's
    offset, and the offset decides the date the institution booked it on. *)
type temporal = private
  { value : Temporal.t
  ; stated : string
  }

val of_date : Date.t -> temporal

(** An instant no source text states, printed in UTC. *)
val of_instant : Ptime.t -> temporal

(** Parse a date or an RFC 3339 instant with its offset. *)
val temporal_of_string : string -> temporal Or_error.t

(** The calendar date as stated, at the stated offset. *)
val local_date : temporal -> Date.t

type balance =
  { quantity : Quantity.t
  ; as_of : temporal
  }

type statement =
  { id : string
  ; account : string
  ; currency : Iso4217.t
  ; period_start : temporal
  ; period_end : temporal
  ; opening : balance
  ; closing : balance
  }

type movement =
  { role : string
  ; quantity : Quantity.t
  ; commodity : Commodity.t
  }

type counterparty =
  { name : string option
  ; iban : string option (** As stated: evidence, not validated. *)
  }

type references =
  { servicer : string option
  ; end_to_end : string option
  ; payment : string option
  ; hash : string option
  }

type exchange =
  { source : string
  ; target : string
  ; rate : Quantity.t option
  ; target_quantity : Quantity.t option
  }

type reported =
  { quantity : Quantity.t
  ; symbol : string
  }

(** Enrichment. A later import may add to it but may not change it. *)
type details =
  { descriptions : string list
  ; movement_type : Movement_type.t option
  ; counterparty : counterparty option
  ; references : references option
  ; exchange : exchange option
  ; reported : reported option
  ; fields : (string * string) list
  }

(** Members outside [details] and [statements] are the fixed part: reimporting a
    record whose fixed part differs is refused. *)
type record =
  { id : string
  ; account : string
  ; statements : string list (** Those listing the row; overlapping exports add to it. *)
  ; occurred : temporal
  ; value_on : Date.t option
  ; movements : movement list
  ; fee : Quantity.t option
  ; group : string option
  ; supporting : bool
  ; observation : bool
  ; details : details
  }

type t =
  { institution : string
  ; identity : identity
  ; converter : converter option
  ; sources : source list
  ; accounts : account list
  ; statements : statement list
  ; records : record list
  }

(** Every semantic problem of the document, one per line of the error. *)
val check : t -> unit Or_error.t

(** One document stating what the given documents state, in order, for one
    institution and identity. Files can overlap: what several state is kept once, and
    it is an error if they state it differently, apart from which statements list a
    record, which adds up. *)
val merge : t list -> t Or_error.t

(** Read a records file, plain or compressed with zstd, which is told from its first
    bytes. Each section is {!check}ed, and sections of one institution and identity are
    {!merge}d, so the result has one document per institution. [file] names the input
    in errors. *)
val of_string : ?file:string -> string -> t list Or_error.t

(** One section of a records file, as JSON Lines. *)
val to_string : t -> string

(** A {!to_string} result as a zstd frame with a checksum. *)
val compress : string -> string Or_error.t
