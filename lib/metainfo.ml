open Core
open Bencode

module Info = struct
  type t =
    { piece_length : int
    ; pieces : string list
    ; name : string
    ; length : int
    }
  [@@deriving sexp_of]

  let get_pieces raw =
    let open Option.Let_syntax in
    let%bind raw_s = extract_string raw in
    let count = String.length raw_s / 20 in
    try Some (List.init count ~f:(fun i -> String.sub raw_s ~pos:(20 * i) ~len:20)) with
    | Invalid_argument _ -> None
  ;;

  let of_bencode b =
    let open Option.Let_syntax in
    let%bind piece_length = extract_value "piece length" b >>= extract_int in
    let%bind pieces = extract_value "pieces" b >>= get_pieces in
    let%bind name = extract_value "name" b >>= extract_string in
    let%map length = extract_value "length" b >>= extract_int in
    { piece_length; pieces; name; length }
  ;;
end

type t =
  { info : Info.t
  ; announce : string
  ; comment : string option
  }
  [@@deriving sexp_of]

let of_bencode b =
  let open Option.Let_syntax in
  let%bind info = extract_value "info" b >>= Info.of_bencode in
  let%map announce = extract_value "announce" b >>= extract_string in
  let comment = extract_value "comment" b >>= extract_string in
  { info; announce; comment }

let to_string t = sexp_of_t t |> Sexp.to_string_hum
