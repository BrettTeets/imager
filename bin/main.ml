open Imagine.Ppm
open Imagine.Mappy.Mappy
open Imagine.Process
open Imagine.Canny

let () = print_endline "reading: "
let new_image : rgb image = Ppm.read "dew_test_really.ppm";;

let () = print_endline "graying: "
let gray = gray_of_image new_image

let () = print_endline "Blur: "
let blur = Process.blur gray

let () = print_endline "Edge: "
let canny = Canny.canny blur 200. 50.

let () = print_endline "write"
let () = ignore @@ Ppm.write "new_canny_2.ppm" canny


