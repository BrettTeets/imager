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

  let read_uint16 ic =
    let b = Bytes.create 2 in
    ignore @@ In_channel.really_input ic b 0 2;
    Bytes.get_uint16_be b 0
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
    Bytes.get_int32_be b 0 |> Int32.to_int

  let read_header ic =
    let b = Bytes.create 4 in
    ignore @@ In_channel.really_input ic b 0 4;
    Bytes.to_string b

  type background_color =
  | Greyscale of int
  | Color of int * int * int
  | Palette of int

  type chunk =
  | IHDR of {width: int ; height : int; bit : int; color : int; compress : int ; filter : int ; interlace : int}
  | SRGB of {color_space : int} (*sRGB*)
  | GAMA of {gamma : int} (*gAMA*)
  (*cHRM, overriden by sRGB and iCCP chunk.*)
  | CHRM of {white_x : int ; white_y : int ; red_x : int ; red_y : int ; green_x : int ; green_y : int ; blue_x : int ; blue_y : int}
  (*bKGD*)
  | BKGD of { color : background_color}
  | IDAT of {length : int}

  let read_IHDR ic =
    let width = read_int32 ic in 
    let height = read_int32 ic in
    let bit = read_uint8 ic in
    let color = read_uint8 ic in
    let compress = read_uint8 ic in
    let filter = read_uint8 ic in
    let interlace = read_uint8 ic in
    IHDR { width ; height ; bit ; color; compress ; filter; interlace}
  ;;

  let read_sRGB ic =
    SRGB {color_space = read_uint8 ic}
  ;;

  let read_cHRM ic =
    CHRM {white_x = read_int32 ic; white_y = read_int32 ic; red_x = read_int32 ic; red_y = read_int32 ic;
          green_x = read_int32 ic; green_y = read_int32 ic; blue_x = read_int32 ic; blue_y = read_int32 ic }
  ;;

  let read_bKGD ic l=
    match l with
    | 2 -> BKGD { color = Greyscale (read_uint16 ic)}
    | 6 -> BKGD { color = Color (read_uint16 ic, read_uint16 ic, read_uint16 ic)}
    | 1 -> BKGD { color = Palette (read_uint8 ic)}
    | _ -> failwith ("This shouldn't be anything but 2 6 and 1. It is: " ^ string_of_int l) 

  let read_IDAT length =
    IDAT {length}
  ;;

  
  (*[Length 4 bytes] [chunk type 4 bytes] [length bytes data] [crc 4 bytes]*)
  let read_chunk ic =
    let l = read_length ic in
    let h = read_header ic in
    let c = (match h with
    | "IHDR" -> read_IHDR ic
    | "sRGB" -> read_sRGB ic
    | "cHRM" -> read_cHRM ic
    | "bKGD" -> read_bKGD ic l
    | "IDAT" -> read_IDAT l
    | _ -> failwith "Not implemented.") in
    ignore @@ read_checksum ic;
    c
  ;;

  let rec read_chunks ic acc =
    let c = read_chunk ic in
    match c with
    | IHDR _ -> let a = c :: acc in read_chunks ic a
    | SRGB _ -> let a = c :: acc in read_chunks ic a
    | CHRM _ -> let a = c :: acc in read_chunks ic a
    | GAMA _ -> let a = c :: acc in read_chunks ic a
    | BKGD _ -> let a = c :: acc in read_chunks ic a
    | IDAT _ -> let a = c :: acc in a

  


  let read path = 
    let ic = In_channel.open_bin path in 
    let bytes = Bytes.create 1 in
      if In_channel.really_input ic bytes 0 1 |> Option.is_none then Error ("Empty Channel for: " ^ path) else 
      if Bytes.get_uint8 bytes 0 != 89 then Error ("Not a png at " ^ path) else
      if discard ic 7 |> Option.is_none then Error ("End of file reached unexpectedly.") else
      let cl = read_chunks in (*This is returning a list of chunks and the pointer is now set to the start of the data stream.*)
      ()

  ;;

  
end