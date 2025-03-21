let logFile = (Out_channel.open_text "log.txt")
let () = Out_channel.output_string logFile "Start of Log."
let logString s = Out_channel.output_string logFile (s ^ "\n")


let file = (In_channel.open_bin) "res/dew.ppm"
let header = Bytes.create 15
let () = ignore @@ In_channel.input file header 0 15

let () = logString "Reading raster."

open Imagine.Mappy.Mappy


(*Test image is w:1200 h:1800*)
let image = create_rgb ~alpha:false 783 738


let rec readBMP channel image width height = 
  (* logString ("width: " ^ (string_of_int width) ^ "height: " ^ (string_of_int height)); *)
  let rgb = Bytes.create 3 in
  ignore @@ In_channel.really_input channel rgb 0 3;
  write_rgb image width height (Bytes.get_uint8 rgb 0) (Bytes.get_uint8 rgb 1) (Bytes.get_uint8 rgb 2);
  if width < image.width-1 then readBMP channel image (width+1) height else
    if height < image.height-1 then readBMP channel image 0 (height+1) else
      image


let printHeader =
  let out = Out_channel.open_bin "dew_test_really.ppm" in Out_channel.output_bytes out header; out

let out = printHeader

let readRGBtoBytes r g b = 
  let bytes = Bytes.create 3 in
  Bytes.set_uint8 bytes 0 r;
  Bytes.set_uint8 bytes 1 g;
  Bytes.set_uint8 bytes 2 b;
  bytes

let () = logString "Writing raster."

let rec writeBMP channel image width height =
  (* logString ("width: " ^ (string_of_int width) ^ "height: " ^ (string_of_int height)); *)
  let rgb = read_rgb image width height readRGBtoBytes in
  Out_channel.output_bytes channel rgb;
  if width < image.width-1 then writeBMP channel image (width+1) height else
    if height < image.height-1 then writeBMP channel image 0 (height+1) else
      image

let writeBMP = writeBMP out

let () = print_endline "Running:";
ignore @@ ((readBMP file image 0 0) |> writeBMP) 0 0;
print_endline "Success?";
()

(* let file = (In_channel.open_bin) "res/dew.ppm"
let fileo = Out_channel.open_bin "new_dew_test_read_4.ppm"


let bb = Bytes.create 15
let () = ignore @@ In_channel.input file bb 0 15
let () = Out_channel.output_bytes fileo bb

let rec sampleRead c = 
  let b = Bytes.create 3 in
  ignore @@ In_channel.really_input file b 0 3;
  Out_channel.output_bytes fileo b;
  if c > 0 then sampleRead (c-1) else ()

let () = sampleRead (783*738) *)


