open Base

(** A CAMT.053 XML reader. It preserves names, attributes, element order and
    values for downstream interpretation. XML formatting and byte-for-byte
    source retention remain the caller's responsibility. *)

type name = string * string

type tree =
  | Element of name * (name * string) list * tree list
  | Text of string

type document =
  { namespace : string
  ; root : tree
  ; statements : tree list
  }

(** Parse a CAMT.053.001 document without applying bank-specific identity,
    balance or accounting policies. The root must contain a statement message
    and at least one statement. DTDs and trailing documents are rejected. *)
val parse : string -> document Or_error.t

(** Direct child elements with the given namespace and local name. *)
val children : namespace:string -> string -> tree -> tree list

(** Typed views retain source text and do not assign institution-specific
    identities or interpret numeric amounts. Unsupported source codes are kept. *)
type date_value =
  | Date of string
  | Date_time of string
  | Both of string * string

type direction =
  | Credit
  | Debit
  | Other_direction of string

type entry_status =
  | Booked
  | Pending
  | Information
  | Other_status of string

type account_id =
  | Iban of string
  | Other_account_id of string

type balance_code =
  | Standard_balance of string
  | Proprietary_balance of string

type amount =
  { value : string
  ; currency : string option
  }

type account =
  { id : account_id option
  ; other_id : string option
  ; currency : string option
  ; servicer_name : string option
  }

type balance =
  { tree : tree
  ; code : balance_code option
  ; proprietary_code : string option
  ; amount : amount option
  ; direction : direction option
  ; as_of : date_value option
  }

type entry =
  { tree : tree
  ; amount : amount option
  ; direction : direction option
  ; status : entry_status option
  ; booking_date : date_value option
  ; value_date : date_value option
  ; account_servicer_reference : string option
  ; proprietary_code : string option
  ; additional_info : string option
  }

type statement =
  { tree : tree
  ; id : string option
  ; account : account option
  ; period_start : date_value option
  ; period_end : date_value option
  ; total_entries : string option
  ; balances : balance list
  ; entries : entry list
  }

(** Interpret a statement tree using standard CAMT fields. Absent optional
    fields remain absent; conflicting singleton fields are rejected. *)
val statement_view : namespace:string -> tree -> statement Or_error.t
