open Base

(** Type rules over a movement's fields, in priority order. *)
type t

type rule = private
  { description : string
  ; account_type : string option
  ; case_sensitive : bool
  ; kind : Movement_type.t
    (** May hold {!Movement_type.placeholder}, which takes the matched value. *)
  ; match_type : string option
  ; compiled : Re.re
  ; field : string option
    (** The field the expression is tested on; the main description if absent. *)
  }

val make_rule
  :  ?field:string
  -> string
  -> string option
  -> bool
  -> string
  -> rule Or_error.t

val with_match_type : rule -> string option -> rule
val rules : t -> rule list
val create : institution:string -> rule list -> t
val institution : t -> string
val empty : string -> t Or_error.t

(** Whether the rule's conditions hold for the movement as the parser decoded it:
    account type, parser-reported type, and the expression on any value of the
    rule's field (the main description by default, see {!Movement.field_values}). *)
val matches : rule -> Movement.t -> bool

(** The value a rule matched, if it matches. *)
val matched_value : rule -> Movement.t -> string option

(** The first rule that matches, with the value it matched. *)
val decide : t -> Movement.t -> (rule * string) option

(** Give the movement the type of the first rule that matches. *)
val apply : t -> Movement.t -> Movement.t
