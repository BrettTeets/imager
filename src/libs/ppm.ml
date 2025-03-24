module type ReadWrite = sig
  val read : string -> Mappy.Mappy.image
  val write : string -> Mappy.Mappy.image -> unit

  val read_rgb_to_grayscale : string -> Mappy.Mappy.image_grayscale_with_theta
  val write_grayscale : string -> Mappy.Mappy.image_grayscale_with_theta -> unit
  val write_theta : string -> Mappy.Mappy.image_grayscale_with_theta -> unit
end

module Ppm : ReadWrite = struct
  open Mappy.Mappy

  let rec _readPPM ic (i:image) w h =
    let rgb = Bytes.create 3 in
    ignore @@ In_channel.really_input ic rgb 0 3;
    write_rgb i w h (Bytes.get_uint8 rgb 0) (Bytes.get_uint8 rgb 1) (Bytes.get_uint8 rgb 2);
    if w < i.width-1 then _readPPM ic i (w+1) h else
      if h < i.height-1 then _readPPM ic i 0 (h+1) else
        i

  let hasP6Header ic = let x = In_channel.input_line ic in match x with
    | None -> failwith "bad input"
    | Some v -> if String.equal v "P6" then true else false
    
  let getWidthHeight ic = let x = In_channel.input_line ic in match x with
    | None -> failwith "bad input"
    | Some v -> let sl = (String.split_on_char ' ' v) in 
      (int_of_string @@ List.nth sl 0, int_of_string @@ List.nth sl 1) 

  let has255Header ic = let x = In_channel.input_line ic in match x with
    | None -> failwith "bad input"
    | Some v -> if String.equal v "255" then true else false

  let read path = 
    let ic = In_channel.open_bin path in
    if hasP6Header ic then () else failwith "bad input";
    let w_h = getWidthHeight ic in
    if has255Header ic then () else failwith "bad input";
    let w = fst w_h in let h = snd w_h in
    let i = create_rgb w h in
    print_string @@ "This: " ^ string_of_int w ^ " " ^ string_of_int h;
    _readPPM ic i 0 0

  let readRGBtoBytes r g b = 
    let bytes = Bytes.create 3 in
    Bytes.set_uint8 bytes 0 r;
    Bytes.set_uint8 bytes 1 g;
    Bytes.set_uint8 bytes 2 b;
    bytes

  let rec _writePPM oc (image:image) width height =
    let rgb = read_rgb image width height readRGBtoBytes in
    Out_channel.output_bytes oc rgb;
    if width < image.width-1 then _writePPM oc image (width+1) height else
      if height < image.height-1 then _writePPM oc image 0 (height+1) else
        ()

  let write path (image:image) =
    let oc = Out_channel.open_bin path in
    Out_channel.output_string oc ("P6\n" ^ string_of_int image.width ^ " " ^ string_of_int image.height ^ "\n255\n");
    _writePPM oc image 0 0


  let rec read_rgb_to_grayscale path = 
    let ic = In_channel.open_bin path in
    if hasP6Header ic then () else failwith "bad input";
    let w_h = getWidthHeight ic in
    if has255Header ic then () else failwith "bad input";
    let w = fst w_h in let h = snd w_h in
    let i = f_create_empty_gray_theta w h in
    (* print_string @@ "This: " ^ string_of_int w ^ " " ^ string_of_int h; *)
    _readPPM2 ic i 0 0

  and _readPPM2 ic (i:image_grayscale_with_theta) w h =
    let rgb = Bytes.create 3 in
    ignore @@ In_channel.really_input ic rgb 0 3;
    (* print_endline "It's in here isn't it"; *)
    f_write_gray_theta i w h (_rgb_bytes_to_lums rgb) 0.;
    (* print_endline ("w: " ^ string_of_int w ^ " h:" ^ string_of_int h ^ "\n") ; *)
    if w < i.width-1 then _readPPM2 ic i (w+1) h else
      if h < i.height-1 then _readPPM2 ic i 0 (h+1) else
        i

  and _rgb_bytes_to_lums rgb =
    let r = float_of_int (Bytes.get_uint8 rgb 0) in
    let g = float_of_int (Bytes.get_uint8 rgb 1) in
    let b = float_of_int (Bytes.get_uint8 rgb 2) in
    (* 0.2126 0.7152 0.0722*)
    r *. 0.2125 +. g *. 0.7152 +. b *. 0.0722


    
  let rec write_grayscale path (image:image_grayscale_with_theta) =
    let oc = Out_channel.open_bin path in
    Out_channel.output_string oc ("P6\n" ^ string_of_int image.width ^ " " ^ string_of_int image.height ^ "\n255\n");
    _writePPM3 oc image 0 0

  and _writePPM3 oc (image:image_grayscale_with_theta) width height =
    let rgb = f_read_gray_theta image readGrayscaleToBytes width height in
    Out_channel.output_bytes oc rgb;
    if width < image.width-1 then _writePPM3 oc image (width+1) height else
      if height < image.height-1 then _writePPM3 oc image 0 (height+1) else
        ()
  
  and readGrayscaleToBytes g _ = 
    let bytes = Bytes.create 3 in
    Bytes.set_uint8 bytes 0 (int_of_float g);
    Bytes.set_uint8 bytes 1 (int_of_float g);
    Bytes.set_uint8 bytes 2 (int_of_float g);
    bytes


    let _scale2 (x:float) =
      ((x -. ~-.180.) /. ((180.) -. (~-.180.))) *. 255.;;
    let _to_degrees x =
      x *. 180. /. Float.pi

  let rec write_theta path (image:image_grayscale_with_theta) =
    let oc = Out_channel.open_bin path in
    Out_channel.output_string oc ("P6\n" ^ string_of_int image.width ^ " " ^ string_of_int image.height ^ "\n255\n");
    _writePPM3 oc image 0 0

  and _writePPM3 oc (image:image_grayscale_with_theta) width height =
    let rgb = f_read_gray_theta image readThetaToBytes width height in
    Out_channel.output_bytes oc rgb;
    if width < image.width-1 then _writePPM3 oc image (width+1) height else
      if height < image.height-1 then _writePPM3 oc image 0 (height+1) else
        ()
  
  and readThetaToBytes _ t = 
    let t = t |> _to_degrees |> _scale2 in
    let bytes = Bytes.create 3 in
    Bytes.set_uint8 bytes 0 (int_of_float t);
    Bytes.set_uint8 bytes 1 (int_of_float t);
    Bytes.set_uint8 bytes 2 (int_of_float t);
    bytes



    
  

end