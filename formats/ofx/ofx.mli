open Base

(** OFX 2.x XML bank statements (the BANKMSGSRSV1 message set).

    A lossless reader: values are kept as written, and each transaction keeps its
    parsed element tree, so callers can derive their own canonical forms. It knows
    the standard, not banks: it does not interpret amounts as accounting values,
    reject corrections, or decide which dates are trustworthy. OFX 1.x SGML,
    credit-card and investment message sets are rejected. *)

(** Elements without attributes or namespaces; text is stripped of surrounding
    whitespace by the reader. *)
type tree =
  | Element of string * tree list
  | Text of string

(** BANKACCTFROM and the statement's CURDEF, as written; all non-empty. *)
type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : string
  }

type transaction =
  { tree : tree (** The STMTTRN element. *)
  ; fitid : string
  ; transaction_type : string (** TRNTYPE *)
  ; posted : string (** DTPOSTED, syntax-checked OFX date *)
  ; user_date : string option (** DTUSER, unchecked: banks fill it loosely *)
  ; amount : string (** TRNAMT as written *)
  ; name : string option
  ; memo : string option
  ; corrects : bool (** CORRECTFITID or CORRECTACTION present *)
  }

type balance =
  { amount : string (** BALAMT as written *)
  ; as_of : string (** DTASOF, syntax-checked *)
  }

type statement =
  { account : account
  ; transactions : transaction list
  ; ledger_balance : balance option
  ; available_balance : balance option
  }

(** Requires successful SONRS/STMTTRNRS statuses and at least one statement. *)
val parse : string -> statement list Or_error.t

(** Reads one STMTTRN element, for instance one stored by a caller. *)
val transaction_of_tree : tree -> transaction Or_error.t

(** The calendar date of an OFX date or datetime ([YYYYMMDD], optionally with
    time, fraction and [\[offset:TZ\]]). A time zone suffix does not move the
    calendar date: banks book in their local calendar. *)
val calendar_date : string -> Ptime.date Or_error.t
