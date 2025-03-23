module Process = struct
  open Mappy.Mappy

  let guassian = [1; 2; 1; 2; 4; 2; 1; 2; 1] (*Normalize by dividing by 16*)
  let sharp = [0; -1; 0; -1; 5; -1; 0; -1; 0]
  let edge = [-1; -1; -1; -1; 8; -1; -1; -1; -1;]

  let edge_v = [-1; 0; 1; -2; 0; 2; -1; 0; 1;]
  let edge_h = [-1; -2; -1; 0; 0; 0; 1; 2; 1;]
  let edge_vf = [-1.;  0.; 1.; -2.; 0.; 2.; -1.; 0.; 1.;]
  let edge_hf = [-1.; -2.; -1.; 0.; 0.; 0.; 1.; 2.; 1.;]

  let _direct x = x;; 

  let guassian5 = [1; 4; 7; 4;1;
                   4;16;26;16;4;
                   7;26;41;26;7;
                   4;16;26;16;4;
                   1; 4; 7; 4;1;] (*normalize by dividing by 273*)
  
  let guassian7 = [0;  0;  1;   2;  1;  0; 0;
                   0;  3; 13;  22; 13;  3; 0;
                   1; 13; 59;  97; 59; 13; 1;
                   2; 22; 97; 159; 97; 22; 2;
                   1; 13; 59;  97; 59; 13; 1;
                   0;  3; 13;  22; 13;  3; 0;
                   0;  0;  1;   2;  1;  0; 0;] (*normalize by 1003*)

  let guassian7F = [0.;  0.;  1.;   2.;  1.;  0.; 0.;
                   0.;  3.; 13.;  22.; 13.;  3.; 0.;
                   1.; 13.; 59.;  97.; 59.; 13.; 1.;
                   2.; 22.; 97.; 159.; 97.; 22.; 2.;
                   1.; 13.; 59.;  97.; 59.; 13.; 1.;
                   0.;  3.; 13.;  22.; 13.;  3.; 0.;
                   0.;  0.;  1.;   2.;  1.;  0.; 0.;] (*normalize by 1003*)

  let rec _kernel3 i p x y k n =
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then write_gray p x y (read_gray i x y (fun x -> x)) else 
    write_gray p x y ((read_kernel 3 i k x y)/n);
    if x < i.width-1 then _kernel3 i p (x+1) y k n else
      if y < i.height-1 then _kernel3 i p 0 (y+1) k n else
        p 

  let _scale (x:float) =
    ((x -. Float.min_float) /. ((Float.max_float) -. (Float.min_float))) *. 255.;;


  let _sobel_help i x y =
    let n = int_of_float @@ Float.sqrt ((Float.pow (float_of_int @@ read_kernel 3 i edge_h x y ) 2.) +. (Float.pow (float_of_int @@ read_kernel 3 i edge_v x y) 2.)) in
    if n > 255 then 255 else n

  let rec _sobel i p x y =
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then write_gray p x y (read_gray i x y (fun x -> x)) else 
    write_gray p x y (_sobel_help i x y);
    if x < i.width-1 then _sobel i p (x+1) y else
      if y < i.height-1 then _sobel i p 0 (y+1) else
        p;;

  let path = "log.txt"
  let oc = Out_channel.open_bin path
  let write_debug = Out_channel.output_string oc
  let debug x = 
    write_debug ("v: " ^ string_of_int x ^ "\n" )
  
  let _to_degrees x =
    x *. 180. /. Float.pi

  let _helperfff x _ = x
  
  let rec _sobel_with_theta i p x y =
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then write_proc2 p x y (read_Proc2 i x y _helperfff) 0. else (
      let grad_x = read_kernelF 3 i edge_hf x y in  write_debug ("gx: " ^ string_of_float grad_x ^ " " );
      let grad_y = read_kernelF 3 i edge_vf x y in  write_debug ("gy: " ^ string_of_float grad_y ^ " " );
      let scaled_h = (Float.hypot grad_x grad_y) in write_debug ("sh: " ^ string_of_float scaled_h ^ " " );
      let theta = Float.atan2 grad_y grad_x in write_debug ("theta: " ^ (string_of_float @@ _to_degrees theta) ^ "\n" ); ();
      write_proc2 p x y scaled_h theta);
    if x < i.width-1 then _sobel_with_theta i p (x+1) y else
      if y < i.height-1 then _sobel_with_theta i p 0 (y+1) else
        p;;

  let _supress i p x y r q =
    let v = (read_gray i x y _direct) in
    if (v >= q  && v >= r) then write_gray p x y (v) else write_gray p x y 0

  let rec non_max_suppression i p x y =
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then write_gray p x y (read_gray i x y (fun x -> x)) else 
    (let _, theta = read_grayG i x y (fun x y -> x, y) in
    let angle = (theta *. 180. /. Float.pi) in
    let angle = (if angle < 0. then angle +. 180. else angle) in
    let _dive = read_gray2 i _direct in
    match angle with                                         (* r               q*)
    | a when (0. <= a && a < 22.5 ) -> _supress i p x y (_dive x (y-1)) (_dive x (y+1))
    | a when (22.5 <= a && a < 67.5) ->  _supress i p x y (_dive (x-1) (y+1)) (_dive (x+1) (y-1))
    | a when (67.5 <= a && a < 112.5) -> _supress i p x y (_dive (x-1) y) (_dive (x+1) y)
    | a when (112.5 <= a && a < 157.5) -> _supress i p x y (_dive (x+1) (y+1)) (_dive (x-1) (y-1))
    | a when (157.5 <= a && a < 180.01) -> _supress i p x y (_dive x (y-1)) (_dive x (y+1)) (*This should be the same as the first.*)
    | a -> failwith ("This value was unaccounted" ^ (string_of_float a) ^ "for. It's a circle how did you escape the circle?"));
    if x < i.width-1 then non_max_suppression i p (x+1) y else
      if y < i.height-1 then non_max_suppression i p 0 (y+1) else
        p;;




  
  let rec _kernel5 i p x y k n =
    if x = 0 || x = 1 || x = i.width-1 || x = i.width-2 || y = 0 || y = 1|| y = i.height-1 || y = i.height-2 then write_gray p x y (read_gray i x y (fun x -> x)) else 
    write_gray p x y ((read_kernel 5 i k x y)/n);
    if x < i.width-1 then _kernel5 i p (x+1) y k n else
      if y < i.height-1 then _kernel5 i p 0 (y+1) k n else
        p;;

  let rec _kernel7 i p x y k n =
    if x = 0 || x = 1 || x = 2 || x = i.width-1 || x = i.width-2 || x = i.width-3 ||
      y = 0 || y = 1 || y = 2 || y = i.height-1 || y = i.height-2 || y = i.height-3 
    then write_gray p x y (read_gray i x y (fun x -> x)) else
      write_gray p x y ((read_kernel 7 i k x y)/n);
      if x < i.width-1 then _kernel7 i p (x+1) y k n else
        if y < i.height-1 then _kernel7 i p 0 (y+1) k n else
          p

  let rec _kernel7F i p x y k n =
    if x = 0 || x = 1 || x = 2 || x = i.width-1 || x = i.width-2 || x = i.width-3 ||
      y = 0 || y = 1 || y = 2 || y = i.height-1 || y = i.height-2 || y = i.height-3 
    then write_proc2 p x y (read_gray i x y (fun x -> float_of_int x)) 0. else
      write_proc2 p x y ((read_kernel_to_F 7 i k x y)/.n) 0.;
      if x < i.width-1 then _kernel7F i p (x+1) y k n else
        if y < i.height-1 then _kernel7F i p 0 (y+1) k n else
          p

  let blur (i:image) = 
    let nm = create_gray i.width i.height in
    _kernel3 i nm 0 0 guassian 16
  
  let blur5 (i:image) =
    let nm = create_gray i.width i.height in
    _kernel5 i nm 0 0 guassian5 273

  let blur7 (i:image) =
    let nm = create_gray i.width i.height in
    _kernel7 i nm 0 0 guassian7 1003

  let blur7F (i:image) =
    match i.pixels with
    | Grey _ -> let nm = create_ProcMap i.width i.height in _kernel7F i nm 0 0 guassian7F 1003.
    | Proc2 _ -> failwith "blur7f unexpected proc 2"
    | RGB _ -> failwith "blur7f unexpected rgb"
    | RGBA _ -> failwith "blur7f unexpected rgba"
    | GreyA _ -> failwith "blur7f unexpected graya"
    | GreyG _ -> failwith "blur7f unexpected grayg"
    

  let sharpen (i:image) = 
    let nm = create_gray i.width i.height in
    _kernel3 i nm 0 0 sharp 1

  let detect_edges (i:image) = 
    let nm = create_gray i.width i.height in
    _kernel3 i nm 0 0 edge 1

  let detect_sobel (i:image) =
    let nm = create_gray i.width i.height in
    _sobel i nm 0 0 
  
  let detect_canny (i:image) = 
    let nm = create_ProcMap i.width i.height in
    (*ignore @@*) _sobel_with_theta i nm 0 0
    (* let nn = create_gray i.width i.height in
    non_max_suppression nm nn 0 0 *)
  

  
end