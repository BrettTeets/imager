open Imagine.Ppm
open Imagine.Mappy.Mappy
open Imagine.Process
open Imagine.Threshold
open Imagine.Draw

let () = print_endline "reading: "
let new_image : rgb image = Ppm.read "binn.ppm";;

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

let rec rrr lines = 
  match lines with
  | [] -> ()
  | h :: t -> Imagine.Shape.Shape.print_line "A" h; rrr t

let () = rrr lines

let () = print_endline "Pruning: "
let rec pruned_lines lines acc =
  match lines with
  | [] -> acc
  | h :: t -> let nac = (Imagine.Shape.Shape.line_approx_rdp h 0.7:: acc) in pruned_lines t nac

let rec float_lines lines acc =
  match lines with
  | [] -> acc
  | h :: t -> let nac = (Imagine.Shape.Shape.line_f_of_int h :: acc) in float_lines t nac

let rec print_nines lines = 
    match lines with
    | [] -> ()
    | h :: t -> Imagine.Shape.Shape.print_line_f "A" h; print_nines t

let fines = float_lines lines []
let nines = pruned_lines fines []

let () = print_nines nines 


let () = print_endline "Drawing Point: "

let doodle = Draw.draw_point colorful (80, 249)  0 255. 0. 0. 
let doodle1 = Draw.draw_point doodle (188, 249)  0 0. 255. 0.
let doodle2 = Draw.draw_point doodle1 (188, 351) 0 0. 255. 0.
let doodle3 = Draw.draw_point doodle2 (79, 351)  0 0. 255. 0.
let doodle4 = Draw.draw_point doodle3 (78, 350)  0 0. 255. 0.
let doodle5 = Draw.draw_point doodle4 (78, 250)  0 0. 255. 0.
let doodle6 = Draw.draw_point doodle5 (79, 249)  0 0. 255. 0.


(*--------------------------*)
(* let () = print_endline "write"
let () = ignore @@ Ppm.write "binary_before_color.ppm" binary
let () = ignore @@ Ppm.write "colored_after_binary.ppm" colorful*)

let () = ignore @@ Ppm.write "marking.ppm" doodle6
