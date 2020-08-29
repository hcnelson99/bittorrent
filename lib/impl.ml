open Core

let fname = "archlinux-2020.08.01-x86_64.iso.torrent"

let main () =
  In_channel.with_file fname ~f:(fun in_channel ->
      let contents = In_channel.input_all in_channel in
      contents
      |> Bencode.of_string
      |> Option.value_map ~default:"Error: could not bdecode" ~f:Bencode.to_string_hum
      |> print_endline)
;;
