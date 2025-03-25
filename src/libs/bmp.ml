(*Terrible bmp decoder*)

module Bmp = struct
  open Mappy.Mappy

  let rec read path width height =
    let ic = In_channel.open_bin path in
    In_channel.seek ic 54L;
    let output = create_rgb width height in 
    _read_rgba ic output (width-1) (height-1)
  and _read_rgba ic image x y =
    let rgba = Bytes.create 4 in
    ignore @@ In_channel.really_input ic rgba 0 4;
    let float_of_byte byte x = float_of_int (Bytes.get_uint8 byte x) in
    (*My sample bmp file is BGR.*)
    write_robust image x y (float_of_byte rgba 2) (float_of_byte rgba 1) (float_of_byte rgba 0);
    if x >= 1 then _read_rgba ic image (x-1) y else
      if y >= 1 then _read_rgba ic image (image.width-1) (y-1) else
        image

;;
end