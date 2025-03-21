(*Work off of ocaml-imagelib right now trying to figure out how all this works.*)
(*Main goal here is to pull together a project and familiarize myself with ocaml and dune.*)
module Mappy = struct
  open Bigarray

  type map 
    = (int, int8_unsigned_elt, c_layout) Array2.t

  (**[create width height] returns a new int array*)
  let create width height 
    = ((Array2.create int8_unsigned c_layout width height):map)
  
  (**[get map width height] returns an int at that position. *)
  let get (m:map) width height 
    = Array2.get m width height

  (**[set map height width value] sets the value at that cooridinate*)
  let set (m:map) h w x 
    = Array2.set m h w x

  (**[fill map color] fills the entire map with a value*)
  let fill (m:map) color
    = Array2.fill m color

  type channels =
  | Grey  of map
  | GreyA of map * map
  | RGB   of map * map * map
  | RGBA  of map * map * map * map

  type image =
  { width   : int
  ; height  : int
  ; max_val : int
  ; pixels  : channels }

  (**[create_rgb width height]*)
  let create_rgb ?(alpha=false) ?(max_val=255) width height =
    let pixels =
      let r = create width height in
      let g = create width height in
      let b = create width height in
      if alpha then
        let a = create width height in
        RGBA (r,g,b,a)
      else RGB (r,g,b)
    in
    { width ; height ; max_val ; pixels }

  let create_gray ?(alpha=false) ?(max_val=255) width height =
    let pixels =
      let g = create width height in
      if alpha then
        let a = create width height in GreyA (g, a)
      else Grey g
    in 
    {width ; height ; max_val ; pixels}

  let read_rgba i x y fn =
    match i.pixels with
    | RGBA(r,g,b,a) ->
        let r = get r x y in
        let g = get g x y in
        let b = get b x y in
        let a = get a x y in
        fn r g b a
    | _ -> failwith "lol"
  
  let read_rgb i x y fn =
    match i.pixels with
    | RGB(r,g,b) ->
        let r = get r x y in
        let g = get g x y in
        let b = get b x y in
        fn r g b
    | _ -> failwith "lol"

  let read_grayA i x y fn =
    match i.pixels with
    | GreyA(g, a) ->
        let g = get g x y in
        let a = get a x y in
        fn g a
    | _ -> failwith "lol"

  let read_gray i x y fn =
    match i.pixels with
    | Grey(g) ->
        let g = get g x y in
        fn g
    | _ -> failwith "lol"

  (** [write_rgba i x y r g b a] writes r g b a and *)
  let write_rgba i x y r g b a =
    match i.pixels with
    | RGBA (rc, gc, bc, ac) -> set rc x y r; set bc x y b; set gc x y g; set ac x y a 
    | _ -> failwith "Nope"

  let write_rgb i x y r g b =
    match i.pixels with
    | RGB (rc, gc, bc) -> set rc x y r; set bc x y b; set gc x y g
    | _ -> failwith "Nope"

  let write_grayA i x y  g a =
    match i.pixels with
    | GreyA (gc, ac) -> set gc x y g; set ac x y a 
    | _ -> failwith "Nope"

  let write_gray i x y  g =
    match i.pixels with
    | Grey (gc) -> set gc x y g 
    | _ -> failwith "Nope"
    
  
end

open Mappy
let action = let x = create 24 24 in set x 5 5 5