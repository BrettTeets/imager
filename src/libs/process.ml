module Process = struct
  open Mappy.Mappy

  let guassian = [1; 2; 1; 2; 4; 2; 1; 2; 1] (*Normalize by dividing by 16*)
  let sharp = [0; -1; 0; -1; 5; -1; 0; -1; 0]
  let edge = [-1; -1; -1; -1; 8; -1; -1; -1; -1;]

  let edge_v = [-1; 0; 1; -2; 0; 2; -1; 0; 1;]
  let edge_h = [-1; -2; -1; 0; 0; 0; 1; 2; 1;]

  let guassian5 = [1; 4; 7; 4;1;
                   4;16;26;16;4;
                   7;26;41;26;7;
                   4;16;26;16;4;
                   1; 4; 7; 4;1;] (*normalize by dividing by 273*)

  let rec _kernel3 i p x y k n =
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then write_gray p x y (read_gray i x y (fun x -> x)) else 
    write_gray p x y ((read_kernel_3_x_3 i x y k)/n);
    if x < i.width-1 then _kernel3 i p (x+1) y k n else
      if y < i.height-1 then _kernel3 i p 0 (y+1) k n else
        p 


  let _sobel_help i x y =
    let n = int_of_float @@ Float.sqrt ((Float.pow (float_of_int @@ read_kernel_3_x_3 i x y edge_h) 2.) +. (Float.pow (float_of_int @@ read_kernel_3_x_3 i x y edge_v) 2.)) in
    if n > 255 then 255 else n

  let rec _sobel i p x y =
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then write_gray p x y (read_gray i x y (fun x -> x)) else 
    write_gray p x y (_sobel_help i x y);
    if x < i.width-1 then _sobel i p (x+1) y else
      if y < i.height-1 then _sobel i p 0 (y+1) else
        p 
  
  let rec _kernel5 i p x y k n =
    if x = 0 || x = 1 || x = i.width-1 || x = i.width-2 || y = 0 || y = 1|| y = i.height-1 || y = i.height-2 then write_gray p x y (read_gray i x y (fun x -> x)) else 
    write_gray p x y ((read_kernel_5_x_5 i x y k)/n);
    if x < i.width-1 then _kernel5 i p (x+1) y k n else
      if y < i.height-1 then _kernel5 i p 0 (y+1) k n else
        p 

  let blur (i:image) = 
    let nm = create_gray i.width i.height in
    _kernel3 i nm 0 0 guassian 16
  
  let blur5 (i:image) =
    let nm = create_gray i.width i.height in
    _kernel5 i nm 0 0 guassian5 273

  let sharpen (i:image) = 
    let nm = create_gray i.width i.height in
    _kernel3 i nm 0 0 sharp 1

  let detect_edges (i:image) = 
    let nm = create_gray i.width i.height in
    _kernel3 i nm 0 0 edge 1

  let detect_sobel (i:image) =
    let nm = create_gray i.width i.height in
    _sobel i nm 0 0 
  
  

  
end