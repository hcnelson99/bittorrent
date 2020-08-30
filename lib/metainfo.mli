module Info : sig
  type t =
    { piece_length : int
    ; pieces : string list
    ; name : string
    ; length : int
    }
  [@@deriving sexp_of]
end

type t =
  { info : Info.t
  ; announce : string
  ; comment : string option
  }
[@@deriving sexp_of]

val of_bencode : Bencode.t -> t option
val to_string : t -> string
