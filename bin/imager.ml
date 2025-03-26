open Imagine.Ppm
open Imagine.Mappy.Mappy
open Imagine.Process
(* 
let () = print_endline "Reading: "
let input = Bmp.read "eevee.bmp" 1200 1800


let () = print_endline "Writing: "
let () = Ppm.write "eevee.ppm" input *)

let () = print_endline "reading: "
let new_image : rgb image = Ppm.read "../../../../dew_really_test.ppm";;

let () = print_endline "graying: "
let gray = gray_of_image new_image

let () = print_endline "THresholding: "
let thresh = Process.threshold gray 120.

let () = print_endline "write"
let () = ignore @@ Ppm.write "../../../thresh.ppm" thresh

(* let () = print_endline "Blurring: "
let blur = Process.blur gray
let () = print_endline "Edge: "
let work = Process.canny blur 200. 50.

(*work*)
let () = print_endline "writing: "
let () = ignore @@ Ppm.write "grayscale.ppm" gray
let () = ignore @@ Ppm.write "blurred.ppm" blur
let () = ignore @@ Ppm.write "supressed.ppm" work *)


(* let i = read "uncanny.ppm";; *)
(* let () = print_endline "making the gray scale: "
let a = Imagine.Mappy.Mappy.make_grayscale i
let () = print_endline "Blurring: "
let b = Imagine.Process.Process.blur7F a
let c = Imagine.Process.Process.sharpen b 
let d = Imagine.Process.Process.detect_sobel b
let () = print_endline "Canny detection: "
let f = Imagine.Process.Process.detect_canny b *)

(* let () = ignore @@ write "kernel_gray.ppm" a
let () = ignore @@ write "kernel_blur_7.ppm" b
let () = ignore @@ write "kernel_sharp.ppm" c
let () = ignore @@ write "kernel_sobel.ppm" d
let () = ignore @@ write "kernel_canny.ppm" f *)


