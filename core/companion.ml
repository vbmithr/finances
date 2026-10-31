open Base

type row =
  { row_id : int64
  ; document_id : int64
  ; institution : string
  ; format : string
  ; row_number : int
  ; fields : string list
  ; context : (string * string) list
  }

type annotation =
  { value_on : Date.t option
  ; display_name : string option
  ; movement_type : Movement_type.t option
  ; reported_type : string option
  ; tags : (string * string) list
  }

type t =
  { format : string
  ; policy : string
  ; match_rows :
      row list -> (int64 * Source_event.t) list -> Bank_match.result list Or_error.t
  ; annotate : row -> annotation Or_error.t
  ; describe : row -> (string * string option) list Or_error.t
  ; fields : row -> (string * string) list Or_error.t
  }
