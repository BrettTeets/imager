open Imagine.Ppm.Ppm

let () = print_endline "reading: "
let i = read "uncanny.ppm";;

Random.init 948783616463728384;;

let () = print_endline "making the gray scale: "
let a = Imagine.Mappy.Mappy.make_grayscale i
let () = print_endline "Blurring: "
let b = Imagine.Process.Process.blur7F a
(* let c = Imagine.Process.Process.sharpen b *)
(* let d = Imagine.Process.Process.detect_sobel b *)
let () = print_endline "Canny detection: "
let f = Imagine.Process.Process.detect_canny b

(*work*)
let () = print_endline "writing: "
let () = ignore @@ write "kernel_gray.ppm" a
let () = ignore @@ write "kernel_blur_7.ppm" b
(* let () = ignore @@ write "kernel_sharp.ppm" c *)
(* let () = ignore @@ write "kernel_sobel.ppm" d *)
let () = ignore @@ write "kernel_canny.ppm" f




