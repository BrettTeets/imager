module Draw = struct
  open Mappy.Mappy
  open Shape



  type colors = float * float * float



  let rec draw_point (img:rgb image) point size r g b =
    let out = copy_rgb img in 
    let x, y = ((Shape.get_x point)), (Shape.get_y point) in
    _draw (size+x) (size+y) x x y out r g b
    
  and _draw width height start_x x y (img:rgb image) r g b=
    (* print_endline @@ "Draw w:" ^ (string_of_int width) ^ " h"^ (string_of_int height) ^ " x:"^ (string_of_int x) ^ " y:" ^ (string_of_int y) ; *)
    write_rgb r g b img x y; 
    if x < width then _draw width height start_x (x+1) y img r g b else
      if y < height then _draw width height start_x start_x (y+1) img r g b else
        img

  ;;

  let rec draw_points img line = 
    match line with
    | [] -> img
    | h :: t -> let r = (draw_point img h 2 255. 0. 0.) in draw_points r t

  let rec draw_lines img (lines:'a Shape.line list) =
    match lines with
    | [] -> img
    | h :: t -> let r = draw_points img (Shape.get_points h) in draw_lines r t

end