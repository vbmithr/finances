open Base
module Db = Rel_sqlite3
open Handle
open Review

type reversal_candidate =
  { original : bank_review
  ; reversal : bank_review
  }

val reversal_candidates : t -> reversal_candidate list Or_error.t

(** Card spending on a bank account that may be a card's settlement: undecided, naming a
    card, with the source accounts of the card accounts that card belongs to and that show
    no payment of it. Checking the accounts is the caller's. *)
val settlement_candidates : t -> (bank_review * string list) list Or_error.t

val reversal_change
  :  t
  -> original:int64
  -> reversal:int64
  -> undo:bool
  -> unit Or_error.t

type reversal_action =
  | Reversal_confirmed
  | Reversal_undone

type reversal_history_entry =
  { id : int64
  ; original : int64
  ; reversal : int64
  ; action : reversal_action
  ; changed_at : Ptime.t
  }

val reversal_history : t -> reversal_history_entry list Or_error.t

(** Explicit same-currency transfer decisions; a confirmation asserts ownership
    of both accounts. Each movement can have at most one confirmed counterpart.
    Rejection of a confirmed pair restores its prior category. All projection
    changes and audit rows commit atomically after verifying stored entries. *)
val set_bank_transfers
  :  ?require_unreviewed:bool
  -> ?expected_previous:Bank_transfer.decision
  -> t
  -> Bank_transfer.decision list
  -> int Or_error.t

type transfer_change =
  { id : int64
  ; first_event_id : int64
  ; second_event_id : int64
  ; previous_state : string option
  ; state : Bank_transfer.state
  ; reason : string
  ; changed_at : Ptime.t
  }

val bank_transfer_history : t -> transfer_change list Or_error.t

type transfer_candidate =
  { first_event_id : int64
  ; second_event_id : int64
  ; first : Source_event.t
  ; second : Source_event.t
  ; first_transaction : Movement.t
  ; second_transaction : Movement.t
  ; days_apart : int
  ; state : Bank_transfer.state option
  ; reason : string option
  ; unique : bool
  ; iban_compatible : bool
  ; iban_match : bool
  ; iban_evidence : string list
  }

val transfer_candidates
  :  ?include_reviewed:bool
  -> t
  -> transfer_candidate list Or_error.t

type bank_account_info =
  { mapping : Bank_account_mapping.t
  ; iban : Iban.t option
  ; iban_source : string
  ; currency : Iso4217.t
  ; movements : int
  ; first_on : Date.t
  ; last_on : Date.t
  ; settlement_account : Bank_account_config.reference option
  }

val bank_account_ids : t -> (string * string * string) list Or_error.t
val bank_account_info : t -> bank_account_info list Or_error.t

(** Preview and confirm a single bank debit against its linked credit card. *)
val card_settlement_preview
  :  ?card_id:string
  -> t
  -> int64
  -> (bank_review * bank_account_info) Or_error.t

val confirm_card_settlement
  :  ?card_id:string
  -> ?expected_ledger:string
  -> t
  -> int64
  -> unit Or_error.t

val undo_card_settlement : t -> int64 -> unit Or_error.t

type card_settlement_action =
  | Confirmed
  | Undone

type card_settlement_change =
  { id : int64
  ; event_id : int64
  ; card_institution : string
  ; card_source_account : string
  ; ledger_account : string
  ; action : card_settlement_action
  ; changed_at : Ptime.t
  }

val card_settlement_history : t -> card_settlement_change list Or_error.t

val confirm_card_settlement_in_transaction
  :  ?card_id:string
  -> ?expected_ledger:string
  -> ?reason:String.t
  -> ?validated_card:bank_account_info
  -> ?source_event:Source_event.t
  -> Handle.t
  -> Int64.t
  -> (unit, Error.t) Result.t

val account_representatives : (Int64.t * Source_event.t) List.t -> Int64.t Hash_set.t

val selected_movements
  :  Handle.t
  -> Int64.t Hash_set.t
  -> ((Int64.t, Movement.t) Hashtbl.t, Error.t) Result.t

(** Each account's movement count and first and last booking dates, then the number of
    movements and of undecided ones, over the kinds the registry reads as movements. *)
val status_summaries
  :  Handle.t
  -> ( (string * string, Base__Int.t * Date.t * Date.t) Hashtbl.Poly.t * Int.t * Int.t
       , Error.t )
       Result.t

val bank_account_info_from
  :  summaries:(string * string, int * Date.t * Date.t) Hashtbl.Poly.t
  -> Handle.t
  -> (int64 Hashtbl.key * Source_event.t) List.t
  -> (int64, Movement.t) Hashtbl.t
  -> (bank_account_info list, Error.t) Result.t

val candidate_pairs
  :  ?include_reviewed:bool
  -> Handle.t
  -> ((int64 * int64 * Bank_transfer.state option * string option) list, Error.t) Result.t

val transfer_candidates_from
  :  ?include_reviewed:bool
  -> Handle.t
  -> (int64 * Source_event.t) List.t
  -> (Int64.t, Movement.t) Hashtbl.t
  -> (int64 * int64 * Bank_transfer.state option * string option) list
  -> (transfer_candidate List.t, Error.t) Result.t

val find_settlement : string -> string -> string Rel_sql.Stmt.t

val set_bank_transfers_internal
  :  ?require_unreviewed:bool
  -> ?expected_previous:Bank_transfer.decision
  -> ?in_transaction:bool
  -> Handle.t
  -> Bank_transfer.decision List.t
  -> (Base__Int.t, Error.t) Result.t
