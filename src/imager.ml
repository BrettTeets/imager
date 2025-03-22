open Imagine.Ppm.Ppm

let () = print_endline "reading: "
let i = read "res/dew.ppm"

let a = Imagine.Mappy.Mappy.make_grayscale i
let b = Imagine.Process.Process.blur7 a
let c = Imagine.Process.Process.sharpen b
let d = Imagine.Process.Process.detect_sobel c

(*work*)
let () = print_endline "writing: "
let () = ignore @@ write "kernel_gray.ppm" a
let () = ignore @@ write "kernel_blur_7.ppm" b
let () = ignore @@ write "kernel_sharp.ppm" c
let () = ignore @@ write "kernel_sobel.ppm" d




