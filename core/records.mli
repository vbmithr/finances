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

(** A quantity of a commodity, as a source states it. *)
type amount =
  { quantity : Quantity.t
  ; commodity : Commodity.t
  }

type balance_type =
  | Opening
  | Closing
  | Available
  | Day_end (** One balance per day, whichever rows it follows. *)

val balance_type_to_string : balance_type -> string

(** A balance a source states at a point in time. *)
type balance =
  { account : string
  ; balance_type : balance_type
  ; quantity : Quantity.t
  ; commodity : Commodity.t
  ; as_of : temporal
  }

(** A claim that some rows belong together. A source may state a period, what they add up
    to, both or neither. Most sources state no statement at all. *)
type statement =
  { id : string
  ; account : string
  ; period : (temporal * temporal) option
  ; total : amount option
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
  ; balance_after : Quantity.t option
    (** The account's balance after this row, as the source displays it. *)
  ; valuations : amount list (** What the source says this row is worth, as stated. *)
  ; names : (string * string) list
    (** What the source calls each symbol a movement reports, such as a token's name for
        its contract: enrichment, never identity. *)
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
  ; balances : balance list
  ; records : record list
  }

(** Every semantic problem of the document, one per line of the error. A document with one
    source lists rows in that source's order, oldest or newest first, so each account's
    [balance_after]s must add up in one direction; a merged document joins exports that can
    have gaps between them, so its balances need not. *)
val check : t -> unit Or_error.t

(** One document stating what the given documents state, in order, for one
    institution and identity. Files can overlap: what several state is kept once, and
    it is an error if they state it differently, apart from which statements list a
    record, which adds up, and what one file says of an account and another does not. *)
val merge : t list -> t Or_error.t

(** {!check} each document, then {!merge} what can be one document: one result per institution and identity, in order of
    first appearance. A source with row IDs and one without cannot share a section. *)
val merge_all : t list -> t list Or_error.t

(** Decode individual tagged lines with the same codecs as a records file. *)
val account_of_line : string -> account Or_error.t

val statement_of_line : string -> statement Or_error.t
val balance_of_line : string -> balance Or_error.t
val record_of_line : string -> record Or_error.t

(** Validate metadata and a document, then merge its repeated evidence as a file reader does. *)
val validate_document : t -> t Or_error.t

(** Read a records file, plain or compressed with zstd, which is told from its first
    bytes. Its sections go through {!merge_all}. [file] names the input
    in errors. *)
val of_string : ?file:string -> string -> t list Or_error.t

(** Whether this is a records file, plain or compressed, of this institution. Only its first
    line is read. *)
val is_records_of : institution:string -> string -> bool

(** One record with its account, as stored for a source event. *)
val row_to_string : account -> record -> string

val row_of_string : string -> (account * record) Or_error.t

(** What identifies a stored row, if this is one: its account, its ID and its fixed part. A digest
    of this survives a converter that later keeps more detail about the row. *)
val row_identity : string -> string option

(** What tells a stored payload apart from another: a row's [row_identity], else the
    payload itself. Two exports of one record that state different [details], such as a
    name a later converter gives, are one. *)
val identity_text : string -> string

(** [restates ~stored payload]: [payload] states the record [stored] does, as a later export
    or converter may. They have one [identity_text], and every member of [details] that
    [stored] states, [payload] states alike; [payload] may state members [stored] lacks, such
    as a name an earlier converter did not give. Any other payload must be [stored]. *)
val restates : stored:string -> string -> bool

(** One section of a records file, as JSON Lines. *)
val to_string : t -> string

(** A {!to_string} result as a zstd frame with a checksum. *)
val compress : string -> string Or_error.t
