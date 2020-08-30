open Core

let fname = "bbb_sunflower_1080p_30fps_normal.mp4.torrent"

let main () =
  In_channel.with_file fname ~f:(fun in_channel ->
      let open Option.Let_syntax in
      let contents = In_channel.input_all in_channel in
      contents
      |> Bencode.of_string
      >>= Metainfo.of_bencode
      |> Option.value_map ~default:"Error: could not bdecode" ~f:Metainfo.to_string
      |> print_endline)
;;
