module type ReadWrite = sig
(* open Mappy.Mappy *)
  (* val read : string -> 
  val write : string -> _ image -> unit *)
end

module Ppm  = struct
  open Mappy.Mappy
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

  let rec read path = 
    let ic = In_channel.open_bin path in
    if hasP6Header ic then () else failwith "bad input";
    let w_h = getWidthHeight ic in
    if has255Header ic then () else failwith "bad input";
    let w = fst w_h in let h = snd w_h in
    let i : rgb image = create_rgb w h in
    _readPPM ic i 0 0
  and _readPPM ic (i:rgb image) w h =
    let float_of_byte byte x = float_of_int (Bytes.get_uint8 byte x) in
    let rgb = Bytes.create 3 in
    ignore @@ In_channel.really_input ic rgb 0 3;
    write_robust i w h (float_of_byte rgb 0) (float_of_byte rgb 1) (float_of_byte rgb 2);
    if w < i.width-1 then _readPPM ic i (w+1) h else
      if h < i.height-1 then _readPPM ic i 0 (h+1) else
        i

  let rec write path (image: _ image) =
    let oc = Out_channel.open_bin path in
    Out_channel.output_string oc ("P6\n" ^ string_of_int image.width ^ " " ^ string_of_int image.height ^ "\n255\n");
    _writePPM oc image 0 0
  and _writePPM oc (image:_ image) width height =
    let rgb = read_robust image readRGBtoBytes width height  in
    Out_channel.output_bytes oc rgb;
    if width < image.width-1 then _writePPM oc image (width+1) height else
      if height < image.height-1 then _writePPM oc image 0 (height+1) else
        ()
  and readRGBtoBytes r g b = 
    let bytes = Bytes.create 3 in
    Bytes.set_uint8 bytes 0 (int_of_float r);
    Bytes.set_uint8 bytes 1 (int_of_float g);
    Bytes.set_uint8 bytes 2 (int_of_float b);
    bytes
end