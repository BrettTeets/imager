module Shape = struct

  type 'a point = 'a * 'a
  let make (x) (y) = ((x, y):_ point)
  let get_x point = fst (point:_ point)
  let get_y point = snd (point:_ point)

  let point_f_of_int (p:int point) =  ((float_of_int @@ fst p, float_of_int @@ snd p):float point)
  let point_i_of_float (p:float point) =  ((int_of_float @@ fst p, int_of_float @@ snd p):int point)

  let add a b = (((fst b + fst a), (snd b + snd a)):_ point)
  let add_f a b = (((fst b +. fst a), (snd b +. snd a)):_ point)

  let sub a b = (((fst b - fst a), (snd b - snd a)):_ point)
  let sub_f a b = (((fst b -. fst a), (snd b -. snd a)):_ point)

  let mul a b = (((fst b * fst a), (snd b * snd a)):_ point)
  let mul_f a b = (((fst b *. fst a), (snd b *. snd a)):_ point)

  let dot_f a b = (fst b *. fst a) +. (snd b *. snd a)


  let equal (a:_ point) (b:_ point) = fst a = fst b && snd a = snd b

  let print_point str (p:int point) = print_endline @@ str ^ "x: " ^ (string_of_int (fst p)) ^ " y: " ^ (string_of_int (snd p))
  let print_point_f str (p:float point) = print_endline @@ str ^ "x: " ^ (string_of_float (fst p)) ^ " y: " ^ (string_of_float (snd p))
 

  let normalization (p:float point) = let x, y = fst p, snd p in let m = hypot x y in ((x /. m, y *. m):float point)

  let distance (a:float point) (b:float point) = let x1, y1, x2, y2 = fst a, snd a, fst b, snd b in hypot (x2 -. x1) (y2 -. y1) 

  let mag_f (p:float point) = let x, y = fst p, snd p in hypot x y

  let vector_of a b = sub_f a b

  let normal_vector_of a b = vector_of a b |> normalization

  let angle_of_normals a b = Float.acos ((dot_f a b) /.(mag_f a *. mag_f b))

  (**Checks where b is within some bounds of a, by epsilon.*)
  let within_bounds a b e = let d = b -.a in (d < e && d > (~-.e))

  let perpendicular_distance (p1:float point) (p2:float point) (p3:float point) = let x1, y1, x2, y2, target_x, target_y = fst p1, snd p1, fst p2, snd p2, fst p3, snd p3 in
    (abs_float @@ (x1-.x2) *. (y1-.target_y) -. (y1-.y2)*.(x1-.target_x)) /. hypot (x1-.x2) (y1-.y2)


  


    

  type 'a line = 'a point list * bool

  let make_empty_line = (([], false):_ line)
  let make_line l b = ((l, b):_ line)
  let rec line_f_of_int (line:int line) = 
    (((_loop (fst line) [] |> List.rev), snd line):float line)
  and _loop line acc =
    match line with
    | [] -> acc
    | h :: t -> let n = (point_f_of_int h) :: acc in _loop t n 


  let get_points l = fst (l:_ line)

  let is_loop l = snd (l:_ line)

  let get_lenth (l:_ line) = fst l |> List.length

  let rec print_line str a = 
    print_endline @@ str ^ "length: " ^ (a |> get_lenth |> string_of_int) ^ "polygon: " ^ (is_loop a |> string_of_bool);
      _print @@ get_points a
  and _print points =
    match points with
    | [] -> ()
    | h :: t -> print_point "\tpoint: " h; _print t

  let rec print_line_f str a = 
    print_endline @@ str ^ "length: " ^ (a |> get_lenth |> string_of_int) ^ "polygon: " ^ (is_loop a |> string_of_bool);
      _print @@ get_points a
  and _print points =
    match points with
    | [] -> ()
    | h :: t -> print_point_f "\tpoint: " h; _print t


  (*Ramer-Douglas-Peucker Algorithm*)
  let rec line_approx_rdp (list:float line) epsilon =
    let arr = Array.of_list (fst list) in
    let len = Array.length arr in
    if len <= 0 then make_empty_line else let p = fst list |> List.hd in ([p] @ loop arr 0 (len-1) epsilon), snd list
  and loop a first_index last_index epsilon =
    print_string @@ "testing: " ^ string_of_int first_index ^ ", " ^ string_of_int last_index;
  let total = last_index - first_index in
  if total < 0 then (print_endline "blank:"; []) else
  if total = 0 then (print_endline "point:"; [Array.get a first_index]) else
  let first, last = Array.get a first_index, Array.get a last_index in
  if total = 1 then (print_endline "double:"; [last]) else
    let perp_fn = perpendicular_distance first last in
    let dist, index = find_furthest a (first_index+1) last_index perp_fn 0. 0 in
    if dist < epsilon then (print_endline "cleared:"; [last]) else 
      (print_endline "break:"; (loop a first_index index epsilon) @ (loop a index last_index epsilon))
  and find_furthest a f l fn acc acc2 =
    if f < l then (let v = fn (Array.get a f) in
      if v >= acc then find_furthest a (f+1) l fn v f
      else find_furthest a (f+1) l fn acc acc2)
    else acc, acc2
  ;;

end