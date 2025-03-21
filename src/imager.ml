open Imagine.Ppm.Ppm

let () = print_endline "reading: "
let i = readPPM "res/dew.ppm"

(*work*)
let () = print_endline "writing: "
let () = ignore @@ writePPM "imagine_test.ppm" i




