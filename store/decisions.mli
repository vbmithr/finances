open Base

(** Everything a person decided, as {!Decisions_portable}: exporting it and applying it to a
    database holding the same statements. *)

val export : Handle.t -> Decisions_portable.t Or_error.t

type changes =
  { accounts : int
  ; assets : int
  ; cards : int
  ; categories : int
  ; rules : int
  ; assignments : int
  ; types : int
  ; resolutions : int
  ; settlements : int
  ; reversals : int
  ; exchanges : int
  ; transfers : int
  }

type outcome =
  | Applied of changes
  | Declined of changes

(** Apply every section in dependency order, as one transaction. [confirm] sees what would
    change and is not asked when nothing would; refusing rolls everything back, as does any
    problem, which is an error listing all a step found. *)
val restore
  :  Handle.t
  -> Decisions_portable.t
  -> confirm:(changes -> bool Or_error.t)
  -> outcome Or_error.t
