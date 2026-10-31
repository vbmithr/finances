open Base

type t =
  { institution : string
  ; name : string
  ; priority : int
  ; pattern : string
  ; case_sensitive : bool
  ; account_type : string option
  ; match_type : string option
  ; field : string option
  ; target : string
  }
