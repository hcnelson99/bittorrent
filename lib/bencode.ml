open Core

type t =
  | String of string
  | Int of int
  | List of t list
  (* Keys must be strings and appear in sorted order *)
  | Dict of t String.Map.t

let rec to_string_hum = function
  | String s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | Int i -> Printf.sprintf "%d" i
  | List l -> "[" ^ (l |> List.map ~f:to_string_hum |> String.concat ~sep:"; ") ^ "]"
  | Dict d ->
    "{"
    ^ (d
      |> Map.to_alist ~key_order:`Increasing
      |> List.map ~f:(fun (k, v) -> Printf.sprintf "\"%s\": %s" k (to_string_hum v))
      |> String.concat ~sep:"; ")
    ^ "}"
;;

let take_safe l n =
  let rec go l n acc =
    match n, l with
    | 0, _ -> Some (List.rev acc, l)
    | _, [] -> None
    | _, x :: l -> go l (n - 1) (x :: acc)
  in
  if n < 0 then None else go l n []
;;

let take_until cs t =
  let b, rest = List.split_while ~f:(fun c -> not (Char.equal c t)) cs in
  match rest with
  | [] -> None
  | t' :: rest -> if Char.equal t t' then Some (b, rest) else None
;;

let int_of_char_list chars =
  try Some (String.of_char_list chars |> Int.of_string) with
  | Failure _ -> None
;;

let of_string s =
  let open Option.Let_syntax in
  let rec go cs =
    let rec go_list = function
      | [] -> None
      | c :: rest ->
        (match c with
        | 'e' -> Some ([], rest)
        | _ ->
          let%bind elt, rest = go (c :: rest) in
          let%bind elts, rest = go_list rest in
          Some (elt :: elts, rest))
    in
    match cs with
    | [] -> None
    | 'l' :: rest ->
      let%bind elts, rest = go_list rest in
      return (List elts, rest)
    | 'i' :: rest ->
      let%bind i, rest = take_until rest 'e' in
      let%map result = int_of_char_list i in
      Int result, rest
    | 'd' :: rest ->
      let%bind elts, rest = go_list rest in
      let%bind alist =
        elts
        |> List.chunks_of ~length:2
        |> List.map ~f:(function
               | [ String x; y ] -> Some (x, y)
               | _ -> None)
        |> Option.all
      in
      let%bind dict =
        match String.Map.of_alist alist with
        | `Duplicate_key _ -> None
        | `Ok dict -> Some dict
      in
      return (Dict dict, rest)
    | c :: rest ->
      if Char.is_digit c
      then (
        let%bind len_chars, rest = take_until (c :: rest) ':' in
        let%bind len = int_of_char_list len_chars in
        let%bind s, rest = take_safe rest len in
        Some (String (String.of_char_list s), rest))
      else None
  in
  let%bind res, rest = go (String.to_list s) in
  if List.is_empty rest then return res else None
;;

let bdecode_and_print s =
  s |> of_string |> Option.value_map ~default:"Error" ~f:to_string_hum |> print_endline
;;

let%expect_test _ =
  let tests =
    [ "i3e"
    ; "i-3e"
    ; "i10001e"
    ; "i0e"
    ; "4:spam"
    ; "l4:spam4:eggse"
    ; "d3:cow3:moo4:spam4:eggse"
    ; "d4:spaml1:a1:bee"
    ]
  in
  tests |> List.iter ~f:bdecode_and_print;
  [%expect
    {|
    3
    -3
    10001
    0
    "spam"
    ["spam"; "eggs"]
    {"cow": "moo"; "spam": "eggs"}
    {"spam": ["a"; "b"]}
  |}]
;;

let%expect_test _ =
  let failing_tests =
    [ "5:spam"; "3:spam"; "d3:cow3:moo4:spame"; "di3e4:spame"; "ite" ]
  in
  failing_tests |> List.iter ~f:bdecode_and_print;
  [%expect {|
    Error
    Error
    Error
    Error
    Error
  |}]
;;
