open Core

type t =
  | String of string
  | Int of int
  | List of t list
  | Dict of t String.Map.t

val to_string_hum : t -> string
val of_string : string -> t option

(* functions to help deconstruct a value *)
val extract_string : t -> string option
val extract_int : t -> int option
val extract_list : t -> t list option
val extract_value : string -> t -> t option
