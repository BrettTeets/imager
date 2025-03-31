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


  let rec line_approx_rdp (l:_ point list) e =
    let a = Array.of_list l in
    let len = Array.length a in
    if len <= 0 then [] else loop a 0 len e
    

  and loop a f l e =
  let t = l - f in
  if t < 0 then [] else
  if t = 0 then [Array.get a f] else
  let first, last = Array.get a f, Array.get a l in
  if t = 1 then [first; last] else
    let perp_fn = perpendicular_distance first last in
    let dist, index = find_furthest a (f+1) l perp_fn 0. 0 in
    if dist < e then [first; last] else 
      (loop a f index e) @ (loop a index l e)
  and find_furthest a f l fn acc acc2 =
    if f < l then (let v = fn (Array.get a f) in
      if v >= acc then find_furthest a (f+1) l fn v f
      else find_furthest a (f+1) l fn acc acc2)
    else acc, acc2
    


  ;;
    

  type 'a line = 'a point list * bool

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

end