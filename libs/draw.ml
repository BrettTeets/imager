module Draw = struct
  open Mappy.Mappy
  open Point



  type colors = float * float * float



  let rec draw_point (img:rgb image) point size =
    let out = copy_rgb img in 
    let x, y = ((Point.x point)), (Point.y point) in
    _draw (size+x) (size+y) x x y out 255. 0. 0.
    
  and _draw width height start_x x y (img:rgb image) r g b=
    (* print_endline @@ "Draw w:" ^ (string_of_int width) ^ " h"^ (string_of_int height) ^ " x:"^ (string_of_int x) ^ " y:" ^ (string_of_int y) ; *)
    write_rgb r g b img x y; 
    if x < width then _draw width height start_x (x+1) y img r g b else
      if y < height then _draw width height start_x start_x (y+1) img r g b else
        img

end