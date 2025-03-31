module Point = struct
  
  type t = int * int

  type tf = float * float

  let make x y = ((x, y):t)
  let get_x point = fst (point:t) 

  let get_y point = snd (point:t)

  let add (a:t) (b:t) = (((fst a + fst b), (snd a + snd b)):t)

  let sub (a:t) (b:t) = (((fst b - fst a), (snd b - snd a)):t)

  let equal (a:t) (b:t) = (fst a = fst b) && (snd b = snd a)

  let rugged_equal (a:t option) (b:t) = if Option.is_none a then false else equal (Option.get a) b

  let print_point str a = print_endline @@ str ^ "x: " ^ (string_of_int (fst a)) ^ " y: " ^ (string_of_int (snd a))

  
  let floating_point (a:t) = ((fst a |> float_of_int, snd a |>float_of_int):tf)

  let subf (a:tf) (b:tf) = (((fst b -. fst a), (snd b -. snd a)):tf)

  (*Normalization of a vector*)
  let normalization (a:tf) = let x, y = fst a, snd a in let m = hypot x y in ((m *. x, m *. y):tf)
  
  
end

module Line = struct
  
  (*List of points in this line and is it self closing.*)
  type t = Point.t list * bool
  type tf = Point.tf list * bool

  let get_points line = fst (line:t)

  let is_closed line = snd (line:t)

  let length line = get_points line |> List.length

  let rec print_line str (a:t) = 
    print_endline @@ str ^ "length: " ^ (a |> length |> string_of_int) ^ "polygon: " ^ (is_closed a |> string_of_bool)
    (* _print @@ get_points a *)
  and _print points =
    match points with
    | [] -> ()
    | h :: t -> Point.print_point "\tpoint: " h; _print t

    (** [floating line \[\] (fst line) (snd line) ]*)
  let rec floating_line  (b:Point.tf list) (a:Point.t list) (v:bool) = match a with
  | [] -> ((b, v):tf)
  | h :: t -> floating_line ((Point.floating_point h) :: b) t v

  let get_pointsf line = fst (line:tf)

end