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
  ; tags : (string * string) list
  }

type display_name =
  | No_display_name
  | Stored_field of string
  | From_annotation

type t =
  { format : string
  ; policy : string
  ; display_name : display_name
  ; match_rows :
      row list -> (int64 * Source_event.t) list -> Bank_match.result list Or_error.t
  ; annotate : row -> annotation Or_error.t
  ; describe : row -> (string * string option) list Or_error.t
  ; fields : row -> (string * string) list Or_error.t
  }
