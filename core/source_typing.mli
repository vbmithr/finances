open Base

type t

type rule = private
  { description : string
  ; account_type : string option
  ; case_sensitive : bool
  ; kind : Movement_type.t
  ; match_type : string option
  ; compiled : Re.re
  }

(** A rule on the bank's own transaction code ([Movement.reported_code]). *)
type reported_action =
  | Kind of Movement_type.t option (** [None] leaves the movement untyped. *)
  | Other_prefix of string (** The code, prefixed, as a provider-specific type. *)

type reported_rule = private
  { code_pattern : string
  ; code_case_sensitive : bool
  ; code_action : reported_action
  ; code_compiled : Transaction_pattern.t
  }

val make_reported_rule : string -> bool -> reported_action -> reported_rule Or_error.t
val with_match_type : rule -> string option -> rule
val rules : t -> rule list
val reported_rules : t -> reported_rule list

(** [reported] rules are tried in order, the first match deciding. *)
val create : ?reported:reported_rule list -> institution:string -> rule list -> t

(** The type the first matching reported rule gives a code. *)
val reported_type : t -> string -> Movement_type.t option

val make_rule : string -> string option -> bool -> string -> rule Or_error.t
val codec : t Jsont.t
val of_json : string -> t Or_error.t
val to_json : t -> string
val institution : t -> string
val empty : string -> t Or_error.t

(** Whether the rule's conditions hold for the movement as the parser decoded it:
    account type, parser-reported type, and the expression on the first stripped
    description. *)
val matches : rule -> Movement.t -> bool

(** Give the movement the type its code maps to, if any, then the type of the
    first description rule that matches. *)
val apply : t -> Movement.t -> Movement.t
