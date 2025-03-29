module Process = struct
  open Mappy.Mappy

  let _extract_gray x _ = x
  let _extract_theta _ y = y
  let _extract_both x y = x, y 

  let _guassian7F = [0.;  0.;  1.;   2.;  1.;  0.; 0.;
                   0.;  3.; 13.;  22.; 13.;  3.; 0.;
                   1.; 13.; 59.;  97.; 59.; 13.; 1.;
                   2.; 22.; 97.; 159.; 97.; 22.; 2.;
                   1.; 13.; 59.;  97.; 59.; 13.; 1.;
                   0.;  3.; 13.;  22.; 13.;  3.; 0.;
                   0.;  0.;  1.;   2.;  1.;  0.; 0.;] (*normalize by 1003*)

  let rec kernel_7 (i:gray image) p ?(x=3) ?(y=3) k n =
      write_gray p x y ((apply_kernel 7 i k x y)/.n) 0.;
      if x < i.width-4 then kernel_7 i p ~x:(x+1) ~y k n else
        if y < i.height-4 then kernel_7 i p ~x:3 ~y:(y+1) k n else
          p
  
  let blur (i:gray image) =
    let output = create_gray i.width i.height in
      kernel_7 i output _guassian7F 1003.

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