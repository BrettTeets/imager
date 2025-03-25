module Process = struct
  open Mappy.Mappy

  let _extract_gray x _ = x
  let _extract_theta _ y = y
  let _extract_both x y = x, y 

  let edge_vf = [-1.;  0.; 1.; -2.; 0.; 2.; -1.; 0.; 1.;]
  let edge_hf = [-1.; -2.; -1.; 0.; 0.; 0.; 1.; 2.; 1.;]

  let _guassian7F = [0.;  0.;  1.;   2.;  1.;  0.; 0.;
                   0.;  3.; 13.;  22.; 13.;  3.; 0.;
                   1.; 13.; 59.;  97.; 59.; 13.; 1.;
                   2.; 22.; 97.; 159.; 97.; 22.; 2.;
                   1.; 13.; 59.;  97.; 59.; 13.; 1.;
                   0.;  3.; 13.;  22.; 13.;  3.; 0.;
                   0.;  0.;  1.;   2.;  1.;  0.; 0.;] (*normalize by 1003*)

  let rec kernel_7 (i:gray image) p x y k n =
    if x = 0 || x = 1 || x = 2 || x = i.width-1 || x = i.width-2 || x = i.width-3 ||
      y = 0 || y = 1 || y = 2 || y = i.height-1 || y = i.height-2 || y = i.height-3 
    then write_gray p x y (read_gray i _extract_gray x y) 0. else
      write_gray p x y ((apply_kernel 7 i k x y)/.n) 0.;
      if x < i.width-1 then kernel_7 i p (x+1) y k n else
        if y < i.height-1 then kernel_7 i p 0 (y+1) k n else
          p
  
  let blur (i:gray image) =
    let output = create_gray i.width i.height in
      kernel_7 i output 0 0 _guassian7F 1003.

  let rec canny (i:gray image) upper lower =
    let work = create_gray i.width i.height in
    ignore @@ _edge_detection_preprocess i work 0 0;
    let work2 = create_gray i.width i.height in
    ignore @@ non_max_suppression work work2 0 0;
    let work3 = create_gray i.width i.height in
    ignore @@ double_thresholding work2 work3 0 0 upper lower;
    let output = create_gray i.width i.height in
    hysteresis work3 output 0 0 
  and _edge_detection_preprocess (i:gray image) (p:gray image) x y =
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then write_gray p x y (read_gray i _extract_gray x y ) 0. else (
      let grad_x = apply_kernel 3 i edge_hf x y in  
      let grad_y = apply_kernel 3 i edge_vf x y in 
      let scaled_h = (Float.hypot grad_x grad_y) in 
      let theta = Float.atan2 grad_y grad_x in
      write_gray p x y scaled_h theta);
    if x < i.width-1 then _edge_detection_preprocess i p (x+1) y else
      if y < i.height-1 then _edge_detection_preprocess i p 0 (y+1) else
        p
  and non_max_suppression (i:gray image) (p:gray image) x y =
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then (let a, b = (read_gray i _extract_both x y) in write_gray p x y a b ) else 
    (let _, theta = read_gray i _extract_both x y in
    let angle = (theta *. 180. /. Float.pi) in
    let angle = (if angle < 0. then angle +. 180. else angle) in
    let _dive = read_gray i _extract_gray  in
    match angle with                                         (* r               q*)
    | a when (-0.1 <= a && a < 22.5 ) -> _supress i p x y (_dive x (y-1)) (_dive x (y+1))
    | a when (22.5 <= a && a < 67.5) ->  _supress i p x y (_dive (x-1) (y+1)) (_dive (x+1) (y-1))
    | a when (67.5 <= a && a < 112.5) -> _supress i p x y (_dive (x-1) y) (_dive (x+1) y)
    | a when (112.5 <= a && a < 157.5) -> _supress i p x y (_dive (x+1) (y+1)) (_dive (x-1) (y-1))
    | a when (157.5 <= a && a < 180.1) -> _supress i p x y (_dive x (y-1)) (_dive x (y+1)) (*This should be the same as the first.*)
    | a -> failwith ("This value was unaccounted" ^ (string_of_float a) ^ "for. It's a circle how did you escape the circle?"));
    if x < i.width-1 then non_max_suppression i p (x+1) y else
      if y < i.height-1 then non_max_suppression i p 0 (y+1) else
        p
  and _supress (i:gray image) (p:gray image) x y r q =
    let v, t = (read_gray i _extract_both x y ) in
    if (v >= q  && v >= r) then write_gray p x y (v) t else write_gray p x y 0. t
  and double_thresholding (i:gray image) (p:gray image) x y upper lower =
    (*I dont know what the max and min potential values are that needs to be resolved or clamped somehow.*)
  let value, theta = read_gray i _extract_both x y in
  if value >= upper then write_gray p x y 255. theta else
    if value >= lower then write_gray p x y 50. theta else
      write_gray p x y 0. 0.;
  if x < i.width-1 then double_thresholding i p (x+1) y upper lower else
    if y < i.height-1 then double_thresholding i p 0 (y+1) upper lower else
      p
  and hysteresis (i:gray image) (p:gray image) x y =
    let value, theta = read_gray i _extract_both x y in
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then write_gray p x y value theta else
      if value < 200. then 
        if _is_strong_at i (x-1) (y-1) || _is_strong_at i (x) (y-1) || _is_strong_at i (x+1) (y-1) ||
           _is_strong_at i (x-1) (y)                                || _is_strong_at i (x+1) (y) ||
           _is_strong_at i (x-1) (y+1) || _is_strong_at i (x) (y+1) || _is_strong_at i (x+1) (y+1)
          then write_gray p x y 255. theta else write_gray p x y 0. theta
      else write_gray p x y value theta;
    if x < i.width-1 then hysteresis i p (x+1) y else
      if y < i.height-1 then hysteresis i p 0 (y+1) else
        p
  and _is_strong_at i x y =
    let v, _ = read_gray i _extract_both x y in v > 250.

  let rec erosion (i:gray image) =
    let o = create_gray i.width i.height in
    _erode i o 2 2
  and _erode i o x y =
    write_gray o x y (compare_kernel 5 i 255. _compare x y) 0.;
    if x < i.width-3 then _erode i o (x+1) y else
      if y < i.width-3 then _erode i o 2 (y+1) else
        o
  and _compare (x:float) (acc:float) = x < acc

  let rec dilation (i:gray image) =
    let o = create_gray i.width i.height in
    _dilate i o 2 2
  and _dilate i o x y =
    write_gray o x y (compare_kernel 5 i 0. _compare x y ) 0.;
    if x < i.width-3 then _dilate i o (x+1) y else
      if y < i.width-3 then _dilate i o 2 (y+1) else
        o
  and _compare (x:float) (acc:float) = x > acc


end