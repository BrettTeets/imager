(*Work off of ocaml-imagelib right now trying to figure out how all this works.*)
(*Main goal here is to pull together a project and familiarize myself with ocaml and dune.*)
module Mappy = struct
  open Bigarray

  (*Currently the main type for storing color information is 8 bit unsigned, the 32 was added for working with angles alongside the pixel for 
  edge detection.*)
  type map8 = (int, int8_unsigned_elt, c_layout) Array2.t
  type mapf = (float, float32_elt, c_layout) Array2.t

  (**[create width height] returns a new int array*)
  let _create8 width height 
    = ((Array2.create int8_unsigned c_layout width height):map8)
  
  (**[create width height] returns a new int array*)
  let _createF width height 
    = ((Array2.create float32 c_layout width height):mapf)
  
  (**[get map width height] returns an int at that position. *)
  let _get (m:map8) width height = Array2.get m width height
  let _getF (m:mapf) width height = Array2.get m width height

  (**[set map height width value] sets the value at that cooridinate*)
  let _set (m:map8) width height x = Array2.set m width height x
  let _setF (m:mapf) width height x =  Array2.set m width height x



  (**[fill map color] fills the entire map with a value*)
  let _fill (m:map8) color
    = Array2.fill m color

  let _copy (m:map8) =
    let w, h = Array2.dim1 m, Array2.dim2 m in
    let nm = _create8 w h in
    Array2.blit m nm; nm

  let _copyF (m:mapf) =
    let w, h = Array2.dim1 m, Array2.dim2 m in
    let nm = _createF w h in
    Array2.blit m nm; nm

  type channels =
  | Grey  of map8
  | GreyA of map8 * map8
  | RGB   of map8 * map8 * map8
  | RGBA  of map8 * map8 * map8 * map8
  | GreyG of map8 * mapf (*This one lets me store both the grayscale color AND the angle of the gradient in the same spot.*)
  | Proc2 of mapf * mapf

  type image =
  { width   : int
  ; height  : int
  ; max_val : int
  ; pixels  : channels }

  (**[create_rgb width height]*)
  let create_rgb ?(alpha=false) ?(max_val=255) width height =
    let pixels =
      let r = _create8 width height in
      let g = _create8 width height in
      let b = _create8 width height in
      if alpha then
        let a = _create8 width height in
        RGBA (r,g,b,a)
      else RGB (r,g,b)
    in
    { width ; height ; max_val ; pixels }

  let create_gray ?(alpha=false) ?(max_val=255) width height =
    let pixels =
      let g = _create8 width height in
      if alpha then
        let a = _create8 width height in GreyA (g, a)
      else Grey g
    in 
    {width ; height ; max_val ; pixels}

  let create_grayG width height =
    let pixels = GreyG ((_create8 width height), (_createF width height)) in
    {width ; height; max_val = 255; pixels}

  let create_ProcMap width height =
    let pixels = Proc2 ((_createF width height), (_createF width height)) in
    {width ; height; max_val = 255; pixels}

  let read_rgba i x y fn =
    match i.pixels with
    | RGBA(r,g,b,a) ->
        let r = _get r x y in
        let g = _get g x y in
        let b = _get b x y in
        let a = _get a x y in
        fn r g b a
    | _ -> failwith "lol"

  let _scale2 (x:float) =
    ((x -. ~-.180.) /. ((180.) -. (~-.180.))) *. 255.;;

  let _random () =
    (Random.float 360.) -. 180.;;
  let _to_degrees x =
    x *. 180. /. Float.pi
  
  let read_rgb i x y fn =
    match i.pixels with
    | RGB(r,g,b) ->
        let r = _get r x y in
        let g = _get g x y in
        let b = _get b x y in
        fn r g b
    | Grey (g) -> let v = _get g x y in fn v v v
    | GreyG (_, f) -> let v = int_of_float @@ (_getF f x y |> _to_degrees) in (*print_endline ("Get: " ^ string_of_int v );*) fn v v v
    | Proc2 (_, f) -> let v = int_of_float @@ (_getF f x y |> _to_degrees |> _scale2) in (*print_endline ("Get: " ^ string_of_int v );*) fn v v v
    | _ -> failwith "read_rgb unsported type."

  let read_grayA i x y fn =
    match i.pixels with
    | GreyA(g, a) ->
        let g = _get g x y in
        let a = _get a x y in
        fn g a
    | _ -> failwith "read_grayA, unsported type."

  let read_grayG i x y fn =
    match i.pixels with
    | GreyG(g, f) ->
        let g = _get g x y in
        let f = _getF f x y in
        fn g f
    | _ -> failwith "read_grayG, unsupported type."

  let read_Proc2 i x y fn =
    match i.pixels with
    | Proc2(gg, ff) -> let g, f = _getF gg x y, _getF ff x y in fn g f
    | _ -> failwith "read_proc2 only supporst proc 2s" 

  let read_gray i x y fn =
    match i.pixels with
    | Grey(g) ->
        let g = _get g x y in
        fn g
    | GreyG (gg, _) -> let g = _get gg x y in fn g
    | _ -> failwith "read_gray unsported type."

  let read_gray2 i fn x y =
    match i.pixels with
    | Grey(g) ->
        let g = _get g x y in
        fn g
    | GreyG (gg, _) -> let g = _get gg x y in fn g
    | _ -> failwith "Attempted to use read_gray2 and passed an unsupported type."

  (** [write_rgba i x y r g b a] writes r g b a and *)
  let write_rgba i x y r g b a =
    match i.pixels with
    | RGBA (rc, gc, bc, ac) -> _set rc x y r; _set bc x y b; _set gc x y g; _set ac x y a 
    | _ -> failwith "Nope"

  let write_rgb i x y r g b =
    match i.pixels with
    | RGB (rc, gc, bc) -> _set rc x y r; _set bc x y b; _set gc x y g
    | _ -> failwith "Nope"

  let write_grayA i x y  g a =
    match i.pixels with
    | GreyA (gc, ac) -> _set gc x y g; _set ac x y a 
    | _ -> failwith "Nope"

  let write_grayG i x y g f =
    match i.pixels with
    | GreyG (gc, ff) -> _set gc x y g; _setF ff x y f 
    | _ -> failwith "Nope"

  let write_proc2 i x y g f =
    match i.pixels with
    | Proc2 (gg, ff) -> _setF gg x y g; _setF ff x y f
    | _ -> failwith "unsupported"

  let write_gray i x y  g =
    match i.pixels with
    | Grey (gc) -> _set gc x y g 
    | _ -> failwith "Nope"
    
  
  let copy i = 
    {i with pixels = (match i.pixels with
    | RGBA (rr, gg, bb, aa) -> RGBA (_copy rr, _copy gg, _copy bb, _copy aa)
    | RGB (rr, gg, bb) -> RGB (_copy rr, _copy gg, _copy bb)
    | GreyA (gg, aa) -> GreyA (_copy gg, _copy aa)
    | Grey (gg) -> Grey (_copy gg)
    | GreyG (gg, rr) -> GreyG (_copy gg, _copyF rr)
    | Proc2 (gg, ff) -> Proc2 (_copyF gg, _copyF ff))}
    

  let _readRGB_to_gray r g b =
    (* 0.2126 0.7152 0.0722*)
  int_of_float((float_of_int r) *. 0.2126 +. 0.7152 *. (float_of_int g) +. 0.0722 *. (float_of_int b))
  
  (*Helper function that loops*)
  let rec _grayscale i n x y =
    write_gray n x y (read_rgb i x y _readRGB_to_gray);
    if x < i.width-1 then _grayscale i n (x+1) y else
      if y < i.height-1 then _grayscale i n 0 (y+1) else
        n

  (*This takes in an image and converts it to grayscale *)
  let make_grayscale (i:image) =
    (*We are creating a image with a gray channel here then using the recursive function to gray it.*)
    let n = create_gray i.width i.height in _grayscale i n 0 0

  let _kernel_help (m:map8) k x y i =
    (_get m x y * List.nth k i)
  
  let rec _apply_kernel size m kernel c x y =
    if c = size*size then 0 else 
    let aid = size / 2 in (*so for 3/2 this is 1 and for 5/2 this is 2, turn it into neg to get the top right corner of the kernel.*)
    let row = c mod size in (*so for 3 0 1 2 are 0 1 2 as are 3 4 and 5 and 6 7 8.*)
    let column = c / size in (*so for 3 0 3 6 are 0, 1 4 7 are 1 and 2 5 8 are 2*)
    (_kernel_help m kernel (x-aid+row) (y-aid+column) c) + (_apply_kernel size m kernel (c+1) x y)
        
  (**[read_kernel size image kernel x y] size must be 3 5 or 7. returns an int.*)
  let read_kernel size image kernel x y = match image.pixels with
    | Grey (gg) -> _apply_kernel size gg kernel 0 x y
    | _ -> failwith "Please use a grayscale image."

    let _kernel_helpF (m:mapf) k x y i =
      (_getF m x y *. List.nth k i)

  let rec _apply_kernelF size m kernel c x y =
    if c = size*size then 0. else 
    let aid = size / 2 in (*so for 3/2 this is 1 and for 5/2 this is 2, turn it into neg to get the top right corner of the kernel.*)
    let row = c mod size in (*so for 3 0 1 2 are 0 1 2 as are 3 4 and 5 and 6 7 8.*)
    let column = c / size in (*so for 3 0 3 6 are 0, 1 4 7 are 1 and 2 5 8 are 2*)
    (_kernel_helpF m kernel (x-aid+row) (y-aid+column) c) +. (_apply_kernelF size m kernel (c+1) x y)
  
  let read_kernelF size image kernel x y = match image.pixels with
    | Proc2 (gg, _) -> _apply_kernelF size gg kernel 0 x y
    | _ -> failwith "Error 21: read_kernelF expects a grayscale float not whatever this is."

  let _kernel_help_to_F (m:map8) k x y i =
    ((float_of_int @@ _get m x y) *. List.nth k i)

  let rec _apply_kernel_to_F size m kernel c x y =
    if c = size*size then 0. else 
    let aid = size / 2 in (*so for 3/2 this is 1 and for 5/2 this is 2, turn it into neg to get the top right corner of the kernel.*)
    let row = c mod size in (*so for 3 0 1 2 are 0 1 2 as are 3 4 and 5 and 6 7 8.*)
    let column = c / size in (*so for 3 0 3 6 are 0, 1 4 7 are 1 and 2 5 8 are 2*)
    (_kernel_help_to_F m kernel (x-aid+row) (y-aid+column) c) +. (_apply_kernel_to_F size m kernel (c+1) x y)

  let read_kernel_to_F size image kernel x y = match image.pixels with
  | Grey (gg) -> _apply_kernel_to_F size gg kernel 0 x y
  | _ -> failwith "Error 21: read_kernelF expects a grayscale float not whatever this is."
    
  



    

end

(* let read_kernel_3_x_3 i x y k = match i.pixels with
    | Grey (gg) -> (_kernel_help gg k (x-1) (y-1) 0) + (_kernel_help gg k x (y-1) 1) + (_kernel_help gg k (x+1) (y-1) 2)
                 + (_kernel_help gg k (x-1)  y    3) + (_kernel_help gg k x  y    4) + (_kernel_help gg k (x+1)  y    5)
                 + (_kernel_help gg k (x-1) (y+1) 6) + (_kernel_help gg k x (y+1) 7) + (_kernel_help gg k (x+1) (y+1) 8)
    | _ -> failwith "Please use a graysacle image." *)

(* let read_kernel_5_x_5 i x y k = match i.pixels with
    | Grey (gg) -> let kh = _kernel_help gg k in 
        (kh (x-2) (y-2) 0) + (kh (x-1) (y-2) 1) + (kh x (y-2) 2) + (kh (x+1) (y-2) 3) + (kh (x+2) (y-2) 4)
      + (kh (x-2) (y-1) 5) + (kh (x-1) (y-1) 6) + (kh x (y-1) 7) + (kh (x+1) (y-1) 8) + (kh (x+2) (y-1) 9)
      + (kh (x-2)  y    10) + (kh (x-1)  y   11) + (kh x  y   12) + (kh (x+1)  y   13) + (kh (x+2)  y   14)
      + (kh (x-2) (y+1) 15) + (kh (x-1) (y+1) 16) + (kh x (y+1) 17) + (kh (x+1) (y+1) 18) + (kh (x+2) (y+1) 19)
      + (kh (x-2) (y+2) 20) + (kh (x-1) (y+2) 21) + (kh x (y+2) 22) + (kh (x+1) (y+2) 23) + (kh (x+2) (y+2) 24)
    | _ -> failwith "Please use a grayscale image." *)