open Base

type posting = private
  { account : string
  ; commodity : string
  ; quantity : Quantity.t
  }

type entry = private
  { date : Date.t
  ; description : string
  ; postings : posting list
  }

val posting : account:string -> commodity:string -> Quantity.t -> posting Or_error.t

(** Requires at least two postings, exactly balanced separately per commodity. *)
val entry : date:Date.t -> description:string -> posting list -> entry Or_error.t
