let file = (In_channel.open_bin) "eevee.bmp"
let () = In_channel.seek file 54L;
(*Test image starts at 54*)


open Mappy.Mappy

(*Test image is w:1200 h:1800*)
let image = create_rgb ~alpha:true 1200 1800

let rec readBMP channel image height width = 
  let rgba = Bytes.create 4 in
  ignore @@ In_channel.input channel rgba 0 4;
  write_rgba image width height (Bytes.get_uint8 rgba 0) (Bytes.get_uint8 rgba 1) (Bytes.get_uint8 rgba 2) (Bytes.get_uint8 rgba 3);
  if height < image.height then readBMP channel image width (height+1) else
    if width < image.width then readBMP channel image (width+1) 0 else
      image
  


let () = print_endline "Running:";
ignore @@ readBMP file image 0 0;
print_endline "Success?"