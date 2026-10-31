open Base

type t =
  { institution : string
  ; aliases : string list
  ; companion_format : string
  ; matching_policy : string
  ; metadata_prefix : string
  ; camt_profile : Camt_profile.t
  ; csv_profile : Csv_clock_profile.t
  }
