module Png = struct

  let discard ic x =
    let b = Bytes.create x in 
    if In_channel.really_input ic b 0 x |> Option.is_none then None else Some ()
  ;;

  let read_int32 ic =
    let b = Bytes.create 4 in
    ignore @@ In_channel.really_input ic b 0 4;
    Bytes.get_int32_be b 0 |> Int32.to_int


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

  let read_IHDR ic _ =
    let width = read_int32 ic in 
    let height = read_int32 ic in
    let bit = 0 in
    let color = 0 in
    let compress = 0 in
    let filter = 0 in
    let interlace = 0 in
    IHDR { width ; height ; bit ; color; compress ; filter; interlace}

  let read_chunk ic =
    let l = read_length ic in
    let h = read_header ic in
    match h with
    | "IHDR" -> read_IHDR ic l
    | _ -> failwith "Not implemented."

  


  let read path = 
    let ic = In_channel.open_bin path in 
    let bytes = Bytes.create 1 in
      if In_channel.really_input ic bytes 0 1 |> Option.is_none then Error ("Empty Channel for: " ^ path) else 
      if Bytes.get_uint8 bytes 0 != 89 then Error ("Not a png at " ^ path) else
      if discard ic 7 |> Option.is_none then Error ("End of file reached unexpectedly.") else
      failwith "Not implemented"

  ;;

  
end