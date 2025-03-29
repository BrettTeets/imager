open Imagine.Ppm
open Imagine.Mappy.Mappy
open Imagine.Process
open Imagine.Threshold
open Imagine.Draw

let () = print_endline "reading: "
let new_image : rgb image = Ppm.read "dew_test_really.ppm";;

let () = print_endline "graying: "
let gray = gray_of_image new_image

let () = print_endline "Blur: "
let blur = Process.blur gray

let () = print_endline "threshing without blur: "
let binary_1 = Threshold.threshold gray 125. 

let () = print_endline "threshing with blur: "
let binary_2 = Threshold.threshold blur 125. 

let () = print_endline "write"
let () = ignore @@ Ppm.write "thresh_1.ppm" binary_1
let () = ignore @@ Ppm.write "thresh_2.ppm" binary_2

let () = print_endline "draw"
let doodle = Draw.draw_point new_image (300, 200) 5
let () = ignore @@ Ppm.write "dotted.ppm" doodle

