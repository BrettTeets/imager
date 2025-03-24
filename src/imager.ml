open Imagine.Ppm.Ppm

let () = print_endline "reading: "
let new_image = read_rgb_to_grayscale "uncanny.ppm";;

let () = print_endline "Blurring: "
let blur = Imagine.Process.Process.f_blur new_image
let () = print_endline "Edge: "
let work = Imagine.Process.Process.f_canny blur

(*work*)
let () = print_endline "writing: "
let () = ignore @@ write_grayscale "grayscale.ppm" new_image
let () = ignore @@ write_grayscale "blurred.ppm" blur
let () = ignore @@ write_grayscale "supressed.ppm" work



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


