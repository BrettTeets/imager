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

  let average_of (a:float point) (b:float point) = let x1, y1, x2, y2 = fst a, snd a, fst b, snd b in ((((x1+.x2)/.2.), ((y1 +. y2)/. 2.)):float point)

  let rec average_of_many (a:float point list) = 
    _loop a 0. 0. 0.
  and _loop a accX accY c =
    match a with
    | [] -> ((accX /. c, accY /. c):float point)
    | h :: t -> _loop t (accX +. (get_x h)) (accY +. (get_y h)) (c +. 1.)

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

  let rec line_i_of_float (line:float line) = 
    (((_loop (fst line) [] |> List.rev), snd line):int line)
  and _loop line acc =
    match line with
    | [] -> acc
    | h :: t -> let n = (point_i_of_float h) :: acc in _loop t n 


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
  let total = last_index - first_index in
  if total < 0 then ( []) else
  if total = 0 then ( [Array.get a first_index]) else
  let first, last = Array.get a first_index, Array.get a last_index in
  if total = 1 then ([last]) else
    let perp_fn = perpendicular_distance first last in
    let dist, index = find_furthest a (first_index+1) last_index perp_fn 0. 0 in
    if dist < epsilon then ( [last]) else 
      ( (loop a first_index index epsilon) @ (loop a index last_index epsilon))
  and find_furthest a f l fn acc acc2 =
    if f < l then (let v = fn (Array.get a f) in
      if v >= acc then find_furthest a (f+1) l fn v f
      else find_furthest a (f+1) l fn acc acc2)
    else acc, acc2
  ;;


  let rec line_collapse (line:float point list) epsilon1 epsilon2 acc = 
    let points = line in 
    let segment, remainder = _loop points epsilon1 [] in
    let simp = line_approx_rdp (segment, true) epsilon2 in
    let nac = acc @ (fst simp |> List.rev) in
    if List.is_empty remainder then nac else
    line_collapse remainder epsilon1 epsilon2 nac
  and _loop points epsilon acc =
  match points with
  | a :: b :: t -> 
    (let base_line = normal_vector_of a b in
    _inner_loop a [b; a] t base_line epsilon)
  | [] -> points, acc
  | _ :: [] -> points, acc
  and _inner_loop root acc line base_line epsilon =
    match line with
    | h :: t -> (
      let test_line = normal_vector_of root h in
      let theta = angle_of_normals base_line test_line in
      if theta < epsilon then _inner_loop root (h :: acc) t base_line epsilon 
        else acc, line
    )
    | [] -> acc, line


  let rec point_collapse (line:float point list) acc epsilon =
    let neighbors, remainder = _loop [] line epsilon in
    let simp = average_of_many neighbors in 
    let nac = simp :: acc in
    if List.is_empty remainder then nac else
      point_collapse remainder nac epsilon
  and _loop acc points epsilon =
    match points with
    | h :: t -> ( if List.is_empty acc then _loop (h :: acc) t epsilon else
      let a = List.hd acc in
      let dist = distance a h in
      if dist > epsilon then acc, points else
        _loop (h :: acc) t epsilon)
    | [] -> acc, points

    let rotate_list l =
      match l with
      | h :: t -> (h :: (t |> List.rev)) |> List.rev
      | _ -> l
      

    let rec set_corner (line:float point list) epsilon1 = 
      if List.length line <= 3 then line else (*If there are only three points and it is a polygon then it is a corner.*) 
      _loop line epsilon1 (List.length line)
    and _loop list eps length=
    if length <= 0 then list else
      match list with
      | a :: b :: c :: _ -> (
        let leg_1 = normal_vector_of a b in
        let leg_2 = normal_vector_of b c in
        let theta = angle_of_normals leg_1 leg_2 in
        let l = rotate_list list in
        if theta < eps then l else (*Eps will be in radians as will theta. If theta is smaller than eps (typically 140 degrees) then you are prob looking at a corner*)
          _loop l eps (length-1) (*Theta is larger than eps, so think 178 degrees of something so probably a straight line, try the next set with the now rotated line.*)
      )
      | _ -> failwith "Na"

      


    (* eps 1 should be 0.17 for max of 10 degrees, that eps is used as part of*)
    (* eps 2 can be like 5. respresenting the distance in pixel from the center line.*)
    (* eps 3 can be like 5 representing a circle around a point that will be groupped up.*)
    (* eps 4 can be like 2.44 for min of 140 degrees before it rejects a corner, this should get up to octogons. drop it 1.74 if you are looking for perfect squares *)
    let reduce_polygon line epsilon1 epsilon2 epsilon3 eps4=
      if Bool.not (snd line) then raise (Invalid_argument "This is not a polygon.") else
        let polygon_points = line_collapse (fst line) epsilon1 epsilon2 [] in
        let polygon_reduced = point_collapse polygon_points [] epsilon3 in
        let polygon_rotated = set_corner polygon_reduced eps4 in
        let polygon_again = line_collapse polygon_rotated epsilon1 epsilon2 [] in
        let polygon_again2 = point_collapse polygon_again [] epsilon3 in
        ((polygon_again2, true):float line)

  


end