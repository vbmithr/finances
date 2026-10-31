(** Group source descriptions that start alike. Statement descriptions usually
    open with a stable operation label ([PRLV], [Card transaction of]) followed
    by merchants, dates and references. This finds the label, so movements that
    no rule types yet can be examined by kind rather than one at a time. *)

(** The first words of the description, lowercased, up to two, stopping at the
    first word that contains a digit. Empty when the description is blank or its
    first word contains a digit. The key only groups; it is not a rule. *)
val key : string -> string

(** A starting regular expression for descriptions that begin with the key's
    words: anchored, tolerant of runs of whitespace, and followed by a word
    boundary, with regular expression characters escaped. Empty for an empty
    key. *)
val pattern : string -> string
