module Canny = struct
open Mappy.Mappy
open Kernel

  let _extract_gray x _ = x
  let _extract_theta _ y = y
  let _extract_both x y = x, y 

  let edge_vf = [-1.;  0.; 1.;
                 -2.; 0.; 2.; 
                 -1.; 0.; 1.;]
  let edge_hf = [-1.; -2.; -1.;
                 0.; 0.; 0.; 
                 1.; 2.; 1.;]


  let rec _loop_3 i o ?(x=1) ?(y=1) fn_inner fn_outer = 
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then fn_outer i o x y else
      fn_inner i o x y;
      if x < i.width-1 then _loop_3 i o ~x:(x+1) ~y fn_inner fn_outer else
        if y < i.height-1 then _loop_3 i o ~x:0 ~y:(y+1) fn_inner fn_outer else
          o

  let rec edge_detection_preprocess i o = 
    _loop_3 i o inner outer
  and inner i o x y =
    let grad_x = change_in_x i x y in  
    let grad_y = change_in_y i x y in 
    let scaled_h = (Float.hypot grad_x grad_y) in 
    let theta = Float.atan2 grad_y grad_x in
    write_gray o x y scaled_h theta
  and outer i o x y =
    write_gray o x y (read_gray i _extract_gray x y ) 0.
  and change_in_x = Kernel.apply_kernel 3 edge_hf 
  and change_in_y = Kernel.apply_kernel 3 edge_vf
  ;;

  let rec non_max_suppression i o =
    _loop_3 i o inner outer
  and inner i o x y =
    let _, theta = read_gray i _extract_both x y in
    let angle = (theta *. 180. /. Float.pi) in
    let angle = (if angle < 0. then angle +. 180. else angle) in
    let _dive = read_gray i _extract_gray  in
    match angle with                                         (* r               q*)
    | a when (-0.1 <= a && a < 22.5 ) -> _supress i o x y (_dive x (y-1)) (_dive x (y+1))
    | a when (22.5 <= a && a < 67.5) ->  _supress i o x y (_dive (x-1) (y+1)) (_dive (x+1) (y-1))
    | a when (67.5 <= a && a < 112.5) -> _supress i o x y (_dive (x-1) y) (_dive (x+1) y)
    | a when (112.5 <= a && a < 157.5) -> _supress i o x y (_dive (x+1) (y+1)) (_dive (x-1) (y-1))
    | a when (157.5 <= a && a < 180.1) -> _supress i o x y (_dive x (y-1)) (_dive x (y+1)) (*This should be the same as the first.*)
    | a -> failwith ("This value was unaccounted" ^ (string_of_float a) ^ "for. It's a circle how did you escape the circle?");
  and _supress (i:gray image) (p:gray image) x y r q =
    let v, t = (read_gray i _extract_both x y ) in
    if (v >= q  && v >= r) then write_gray p x y (v) t else write_gray p x y 0. t
  and outer i o x y =
   let a, b = (read_gray i _extract_both x y) in write_gray o x y a b

  let rec double_thresholding i o upper lower =
    let inner2 = inner upper lower in
    _loop_3 i o inner2 inner2
  and inner upper lower i o x y =
  let value, theta = read_gray i _extract_both x y in
  if value >= upper then write_gray o x y 255. theta else
    if value >= lower then write_gray o x y 50. theta else
      write_gray o x y 0. 0.
  ;;

  let rec hysteresis i o =
    _loop_3 i o inner outer
  and inner   i o x y = 
    let value, theta = read_gray i _extract_both x y in
    if value < 200. then 
      if _is_strong_at i (x-1) (y-1) || _is_strong_at i (x) (y-1) || _is_strong_at i (x+1) (y-1) ||
        _is_strong_at i (x-1) (y)                                || _is_strong_at i (x+1) (y) ||
        _is_strong_at i (x-1) (y+1) || _is_strong_at i (x) (y+1) || _is_strong_at i (x+1) (y+1)
        then write_gray o x y 255. theta else write_gray o x y 0. theta
    else write_gray o x y value theta;
  and outer   i o x y =
    let value, theta = read_gray i _extract_both x y in
    write_gray o x y value theta
  and _is_strong_at i x y =
    let v, _ = read_gray i _extract_both x y in v > 250.
  ;;

  (*Main function to call for canny edge detection.*)
  (**[canny image upper lower] returns a grayscale image withe edges highlighted.*)
  let canny (img:gray image) upper lower =
    let left = create_gray img.width img.height in
    let right = create_gray img.width img.height in
    ignore @@ edge_detection_preprocess img left; (*Writes the preprocess into left.*)
    ignore @@ non_max_suppression left right; (*writes the max suppression into right.*)
    ignore @@ double_thresholding right left upper lower; (*writes the double threshould into left.*)
    hysteresis left right (*Finally applies the hysteresis to right and returns that.*)

  (* and _edge_detection_preprocess (i:gray image) (p:gray image) x y =
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then write_gray p x y (read_gray i _extract_gray x y ) 0. else (
      let grad_x = apply_kernel 3 i edge_hf x y in  
      let grad_y = apply_kernel 3 i edge_vf x y in 
      let scaled_h = (Float.hypot grad_x grad_y) in 
      let theta = Float.atan2 grad_y grad_x in
      write_gray p x y scaled_h theta);
    if x < i.width-1 then _edge_detection_preprocess i p (x+1) y else
      if y < i.height-1 then _edge_detection_preprocess i p 0 (y+1) else
        p
  and _non_max_suppression (i:gray image) (p:gray image) x y =
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
    if x < i.width-1 then _non_max_suppression i p (x+1) y else
      if y < i.height-1 then _non_max_suppression i p 0 (y+1) else
        p
  and _supress (i:gray image) (p:gray image) x y r q =
    let v, t = (read_gray i _extract_both x y ) in
    if (v >= q  && v >= r) then write_gray p x y (v) t else write_gray p x y 0. t
  and _double_thresholding (i:gray image) (p:gray image) x y upper lower =
    (*I dont know what the max and min potential values are that needs to be resolved or clamped somehow.*)
  let value, theta = read_gray i _extract_both x y in
  if value >= upper then write_gray p x y 255. theta else
    if value >= lower then write_gray p x y 50. theta else
      write_gray p x y 0. 0.;
  if x < i.width-1 then _double_thresholding i p (x+1) y upper lower else
    if y < i.height-1 then _double_thresholding i p 0 (y+1) upper lower else
      p
  and _hysteresis (i:gray image) (p:gray image) x y =
    let value, theta = read_gray i _extract_both x y in
    if x = 0 || x = i.width-1 || y = 0 || y = i.height-1 then write_gray p x y value theta else
      if value < 200. then 
        if _is_strong_at i (x-1) (y-1) || _is_strong_at i (x) (y-1) || _is_strong_at i (x+1) (y-1) ||
          _is_strong_at i (x-1) (y)                                || _is_strong_at i (x+1) (y) ||
          _is_strong_at i (x-1) (y+1) || _is_strong_at i (x) (y+1) || _is_strong_at i (x+1) (y+1)
          then write_gray p x y 255. theta else write_gray p x y 0. theta
      else write_gray p x y value theta;
    if x < i.width-1 then _hysteresis i p (x+1) y else
      if y < i.height-1 then _hysteresis i p 0 (y+1) else
        p
  and _is_strong_at i x y =
    let v, _ = read_gray i _extract_both x y in v > 250. *)



end