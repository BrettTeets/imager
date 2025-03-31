module Process = struct
  open Mappy.Mappy
  open Kernel
  
  let blur_kernel_3 = [1.; 2.; 1.;
                       2.; 4.; 2.; 
                       1.; 2.; 1.; ] |> List.map (fun x -> x/.16.) 

  let _guassian7F = [0.;  0.;  1.;   2.;  1.;  0.; 0.;
                   0.;  3.; 13.;  22.; 13.;  3.; 0.;
                   1.; 13.; 59.;  97.; 59.; 13.; 1.;
                   2.; 22.; 97.; 159.; 97.; 22.; 2.;
                   1.; 13.; 59.;  97.; 59.; 13.; 1.;
                   0.;  3.; 13.;  22.; 13.;  3.; 0.;
                   0.;  0.;  1.;   2.;  1.;  0.; 0.;] (*normalize by 1003*)
  ;;
  let _guassian7F = List.map (fun x -> x /. 1003.)  _guassian7F (*Fun bit of performance, went from 1000 ms to do a blur, to 900 ms to do blur with this. With a typical 700 by 700 img *)

  (*TODO: change the seven to a strong blur, make a 5x5 blur, and a 3x3 weak blur.
  maybe guassian blur vs adaptive blur in the future?*)
  
  let rec blur img =
    (*instead of a full copy we could do a per pixel copy and handle the edge case of the kernel not fitting.*)
    let output = copy_gray img in
      blur_loop img output
  and blur_loop ?(x=3) ?(y=3) img out =
    write_gray out x y ((blur_convolution img x y)) 0.;
    if x < img.width-4 then blur_loop ~x:(x+1) ~y img out else
      if y < img.height-4 then blur_loop ~x:3 ~y:(y+1) img out else
        out
  and blur_convolution img x y = (Kernel.apply_kernel 7 _guassian7F img x y)

  
  let rec erosion (img:gray image) =
    let out = create_gray img.width img.height in
    _erode img out 2 2
  and _erode img out x y =
    write_gray out x y (lowest_value img x y) 0.;
    if x < img.width-3 then _erode img out (x+1) y else
      if y < img.width-3 then _erode img out 2 (y+1) else
        out
  and lowest_value img x y = Kernel.compare_kernel 5 (<) img 255. x y

  let rec dilation (img:gray image) =
    let o = create_gray img.width img.height in
    _dilate img o 2 2
  and _dilate img out x y =
    write_gray out x y (heighest_value img x y ) 0.;
    if x < img.width-3 then _dilate img out (x+1) y else
      if y < img.width-3 then _dilate img out 2 (y+1) else
        out
  and heighest_value i x y = Kernel.compare_kernel 5 (>) i 0. x y

end