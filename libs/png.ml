module Png = struct

  let discard ic x =
    let b = Bytes.create x in 
    if In_channel.really_input ic b 0 x |> Option.is_none then None else Some ()
  ;;

  let read_int32 ic =
    let b = Bytes.create 4 in
    ignore @@ In_channel.really_input ic b 0 4;
    Bytes.get_int32_be b 0 |> Int32.to_int
  ;;

  let read_uint8 ic =
    let b = Bytes.create 1 in
    ignore @@ In_channel.really_input ic b 0 1;
    Bytes.get_uint8 b 0
  ;;

  let read_checksum ic = discard ic 4

  let read_length ic =
    let b = Bytes.create 4 in
    ignore @@ In_channel.really_input ic b 0 4;
    Bytes.get_int32_be b 0

  let read_header ic =
    let b = Bytes.create 4 in
    ignore @@ In_channel.really_input ic b 0 4;
    Bytes.to_string b

  type chunk =
  | IHDR of {width: int ; height : int; bit : int; color : int; compress : int ; filter : int ; interlace : int}
  | SRGB of {color_space : int} (*sRGB*)
  | GAMA of {gamma : int}

  let read_IHDR ic =
    let width = read_int32 ic in 
    let height = read_int32 ic in
    let bit = read_uint8 ic in
    let color = read_uint8 ic in
    let compress = read_uint8 ic in
    let filter = read_uint8 ic in
    let interlace = read_uint8 ic in
    IHDR { width ; height ; bit ; color; compress ; filter; interlace}
  let read_sRGB ic =
    SRGB {color_space = read_uint8 ic}

  

  let read_chunk ic =
    let _ = read_length ic in
    let h = read_header ic in
    let c = (match h with
    | "IHDR" -> read_IHDR ic
    | "sRGB" -> read_sRGB ic
    | _ -> failwith "Not implemented.") in
    ignore @@ read_checksum ic;
    c
  ;;

  


  let read path = 
    let ic = In_channel.open_bin path in 
    let bytes = Bytes.create 1 in
      if In_channel.really_input ic bytes 0 1 |> Option.is_none then Error ("Empty Channel for: " ^ path) else 
      if Bytes.get_uint8 bytes 0 != 89 then Error ("Not a png at " ^ path) else
      if discard ic 7 |> Option.is_none then Error ("End of file reached unexpectedly.") else
      failwith "Not implemented"

  ;;

  
end