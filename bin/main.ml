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

(* let rec rrr lines = 
  match lines with
  | [] -> ()
  | h :: t -> Imagine.Shape.Shape.print_line "A" h; rrr t

let () = rrr lines *)


(* eps 1 should be 0.17 for max of 10 degrees, that eps is used as part of*)
    (* eps 2 can be like 5. respresenting the distance in pixel from the center line.*)
    (* eps 3 can be like 5 representing a circle around a point that will be groupped up.*)
    (* eps 4 can be like 2.44 for min of 140 degrees before it rejects a corner, this should get up to octogons. drop it 1.74 if you are looking for perfect squares *)
let () = print_endline "Pruning: "
let rec pruned_lines lines acc =
  match lines with
  | [] -> acc
  | h :: t -> let nac = ((Imagine.Shape.Shape.reduce_polygon h 0.80 3.5 5. 2.44) :: acc) in pruned_lines t nac

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

let rec doodle img list =
  match list with
  | [] -> img
  | h :: t -> let img1 = Draw.draw_point img h 1 255. 0. 0. in doodle img1 t

let rec doodler img list =
  match list with 
  | [] -> img
  | (h:_ Imagine.Shape.Shape.line) :: t -> let h1 = Imagine.Shape.Shape.line_i_of_float h in let ps = (fst h1) in let img1 = doodle img ps in doodler img1 t

let doodles = doodler colorful nines


(*--------------------------*)
(* let () = print_endline "write"
let () = ignore @@ Ppm.write "binary_before_color.ppm" binary
let () = ignore @@ Ppm.write "colored_after_binary.ppm" colorful*)

let () = ignore @@ Ppm.write "marking.ppm" doodles
