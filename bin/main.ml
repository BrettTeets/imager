open Imagine.Ppm
open Imagine.Mappy.Mappy
open Imagine.Process
open Imagine.Threshold
open Imagine.Draw

let () = print_endline "reading: "
let new_image : rgb image = Ppm.read "binnn.ppm";;

let () = print_endline "graying: "
let gray = gray_of_image new_image

let () = print_endline "Blur: "
let blur = Process.blur gray

let () = print_endline "threshing with blur: "
let binary = Threshold.threshold blur 125. 

let () = print_endline "converting to color"
let colorful = rgb_of_binary binary

(*--------------------------*)
open Imagine.Contours
let () = print_endline "here we go...: "
let lines = Contours.find_contour_list binary

  (* let () = List.iter (fun a -> print_endline @@ "x: " ^ (string_of_int (fst a)) ^ " y: " ^ (string_of_int (snd a))) line *)

let annoted = Draw.draw_lines colorful lines
let () = ignore @@ Ppm.write "result.ppm" annoted




let () = print_endline "Drawing Point: "

let doodle = Draw.draw_point colorful (345, 372) 4 255. 0. 0.
let doodle1 = Draw.draw_point doodle (188, 344) 4 0. 255. 0.


(*--------------------------*)
(* let () = print_endline "write"
let () = ignore @@ Ppm.write "binary_before_color.ppm" binary
let () = ignore @@ Ppm.write "colored_after_binary.ppm" colorful*)

let () = ignore @@ Ppm.write "marking.ppm" doodle1 


