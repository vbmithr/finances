open Base

(** Decode directly to the described OCaml type. Repeated object members are
    rejected before typed decoding, including in nested objects. *)
val decode : 'a Jsont.t -> string -> 'a Or_error.t

val encode : ?format:Jsont.format -> 'a Jsont.t -> 'a -> string

(** Turn domain validation failures into Jsont errors for use inside codecs. *)
val or_error : 'a Or_error.t -> 'a

val validated : ?meta:Jsont.Meta.t -> (unit -> 'a) -> 'a
val integer : int Jsont.t
val int64_codec : int64 Jsont.t
val version : int -> unit Jsont.t
val tag : string -> unit Jsont.t
val date : Date.t Jsont.t
val quantity : Quantity.t Jsont.t
val currency : Iso4217.t Jsont.t
val iban : Iban.t Jsont.t

val members
  :  'a Jsont.t
  -> ((string * 'a) list, 'a, (string * 'a) list) Jsont.Object.Mems.map

val string_map : (string * string) list Jsont.t
