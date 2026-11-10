type statement =
  { source_account : string
  ; reference : string option
  ; currency : Iso4217.t
  ; period_start : Temporal.t
  ; period_end : Temporal.t
  ; opening_as_of : Temporal.t
  ; closing_as_of : Temporal.t
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; events : Source_event.t list
  }

type total_claim =
  { source_account : string
  ; reference : string
  ; amount : Records.amount
  ; complete : bool
  ; events : Source_event.t list
  }

type balance =
  { account : string
  ; balance_type : Records.balance_type
  ; commodity : Commodity.t
  ; quantity : Quantity.t
  ; as_of : Temporal.t
  }

type t =
  { statements : statement list
  ; totals : total_claim list
  ; balances : balance list
  ; events : Source_event.t list
  ; companion_rows : (int * string list) list
  ; context : (string * string) list
  }

let empty =
  { statements = []
  ; totals = []
  ; balances = []
  ; events = []
  ; companion_rows = []
  ; context = []
  }
;;
