open Core

type t =
  | String of string
  | Int of int
  | List of t list
  | Dict of t String.Map.t

val to_string_hum : t -> string
val of_string : string -> t option
