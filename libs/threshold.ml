module Threshold = struct
open Mappy.Mappy

  let _extract_gray x _ = x

  let _compare img thresh x y =
    let v = read_gray img _extract_gray x y in
      (* print_string ("Comparing against: " ^ string_of_float v ^ "\n"); *)
      if v > thresh then 255 else 0 


  let rec _threshing_loop x y img output thresh = 
    write_binary (_compare img thresh x y) output x y ;
    if x < img.width-1 then _threshing_loop (x+1) y img output thresh  else
      if y < img.height-1 then _threshing_loop 0 (y+1) img output thresh  else
        output
  let threshing = _threshing_loop 0 0



  let threshold (img:gray image) thresh =
    let output = create_binary img.width img.height in
    threshing img output thresh


end