(*Work off of ocaml-imagelib right now trying to figure out how all this works.*)
(*Main goal here is to pull together a project and familiarize myself with ocaml and dune.*)
module Mappy  = struct
    open Bigarray
  
    type map8 = (int, int8_unsigned_elt, c_layout) Array2.t
    type mapf = (float, float32_elt, c_layout) Array2.t
  
    let _create8 width height = ((Array2.create int8_unsigned c_layout width height):map8)
    let _get8 (m:map8) x y = Array2.get m x y
    let _set8 (m:map8) x y v = Array2.set m x y v
    let _copy8 (m:map8) =
      let w, h = Array2.dim1 m, Array2.dim2 m in
      let output = _create8 w h in
      Array2.blit m output; output

    (*Floating point maps.*)
    let _createf width height = ((Array2.create float32 c_layout width height):mapf)
    let _getf (m:mapf) x y = Array2.get m x y
  
    let _setf (m:mapf) x y = Array2.set m x y
  
    let _fillf (m:mapf) value = Array2.fill m value
  
    (** [_mapf m fn x y] This maps fn to m at x and y. This modifies the image in place.*)
    let _mapf (m:mapf) (fn:'x->'x) x y = _setf m x y (fn @@ _getf m x y )
  
    (** [_zip3 m1 m2 m3 o fn x y] This zips the values of m1 m2 m3 into o at x and y with fn. This does not modify the source maps.*)
    let pix_zip_3_to_1 (m1:mapf) (m2:mapf) (m3:mapf) (o:mapf) fn x y = _setf o x y (fn (_getf m1 x y) (_getf m2 x y) (_getf m3 x y))
  
    let _copyf (m:mapf) = 
      let w, h = Array2.dim1 m, Array2.dim2 m in
      let output = _createf w h in
      Array2.blit m output; output

    
  
    let lums_of_rgb r g b =
      r *. 0.2125 +. g *. 0.7152 +. b *. 0.0722
  
  
    (*Representing the different types of channels with polymorphic data types.*)
    type rgba = [ | `RGBA of mapf * mapf * mapf * mapf]
    type rgb = [ | `RGB of mapf * mapf * mapf]
    type gray = [ | `GRAY of mapf * mapf ]
    type binary = [ | `Binary of map8]
  
    type channel = [ rgb | gray | rgba | binary]
  
    type 'a image = {width : int; height : int; pixels : ([< channel] as 'a)} 
  
    let create_rgba width height =
      let make () = _createf width height in
      {width ; height; pixels = `RGBA (make (), make (), make (), make ())}
    ;;
    let create_rgb width height =
      let make () = _createf width height in
      {width ; height; pixels = `RGB (make (), make (), make ())}
    ;;
    let create_gray width height =
      let make () = _createf width height in
      {width ; height; pixels = `GRAY (make (), make ())}
    ;;
    let create_binary width height = 
      let make () = _create8 width height in
      {width; height; pixels = `Binary (make ())}
    
  
    (*Pixel Level Operations.*)
  
    (**[read_robust i fn x y] Read robust will take in any image and attempt apply fn to it at x and y. An alpha channel will be ignored.
    In the case of grayscale image the gray values will be duplicated thrice.*)
    let read_robust (i:_ image) fn x y =
      match i.pixels with
      | `RGBA (rr, gg, bb, _) | `RGB (rr, gg, bb) -> 
          let r = _getf rr x y in
          let g = _getf gg x y in
          let b = _getf bb x y in
          fn r g b
      | `GRAY (gg, _) -> let g = _getf gg x y in fn g g g
      | `Binary (bb) -> let b = if (_get8 bb x y) > 0 then 255. else 0. in fn b b b
    ;;
  
    (**[write_robust i x y a r g b] Write robust will take any image and attempt to write r g b and optionally a values to it's color channels.
      in the case of grey image it will calculate the luminosity from the given r g b and apply that as the gray value.*)
    let write_robust i x y ?(a=255.) r g b  =
      match i.pixels with
      | `RGBA (rr, gg, bb, aa) -> _setf rr x y r; _setf gg x y g; _setf bb x y b; _setf aa x y a
      | `RGB (rr, gg, bb) -> _setf rr x y r; _setf gg x y g; _setf bb x y b
      | `GRAY(gg, _) -> let v = lums_of_rgb r g b in _setf gg x y v (*Convert rgb to a single luminosity value*)
      | `Binary (bb) -> _set8 bb x y (if r > 0. || b > 0. || g > 0. then 1 else 0) 
  
    let read_gray (i:gray image) fn x y =
      match i.pixels with
      | `GRAY (gg, tt) -> let g, t = _getf gg x y, _getf tt x y in fn g t
  
    let write_gray (i:gray image) x y v t=
      match i.pixels with
      | `GRAY (gg, tt) -> _setf gg x y v; _setf tt x y t

    let read_binary fn (i:binary image) x y =
      match i.pixels with
      | `Binary (bb) -> let b = _get8 bb x y in fn b
    ;;

    let write_binary v (i:binary image)  x y =
      match i.pixels with
      | `Binary (bb) -> _set8 bb x y v
    
  
    (*Kernel level Operations.*)
  
    (** [apply_kernel_grayscale size image kernel x y] size of the kernel is 3 5 or 7, image must be grayscale, kernel must be of floats, x and y is the center point.
    returns the the result of apply the kernel to the given image. Does not overwrite.*)
    let rec apply_kernel size (image:gray image) kernel x y = match image.pixels with
      | `GRAY (gg, _) -> _kernel_applicator size gg kernel 0 x y
    and _kernel_applicator size m kernel c x y =
      if c = size*size then 0. else 
      let aid = size / 2 in (*so for 3/2 this is 1 and for 5/2 this is 2, turn it into neg to get the top right corner of the kernel.*)
      let row = c mod size in (*so for 3 0 1 2 are 0 1 2 as are 3 4 and 5 and 6 7 8.*)
      let column = c / size in (*so for 3 0 3 6 are 0, 1 4 7 are 1 and 2 5 8 are 2*)
      (_apply m kernel (x-aid+row) (y-aid+column) c) +. (_kernel_applicator size m kernel (c+1) x y)
    and _apply (m:mapf) k x y i =    
      (_getf m x y *. List.nth k i)

    (*The fn should be doing a compare of float float to bool*)
    let rec compare_kernel size (image:gray image) start fn x y = match image.pixels with
    | `GRAY (gg, _) -> _kernel_applicator size gg fn 0 x y start
    and _kernel_applicator size map fn c x y acc =
      if c = size*size then acc else
      let aid = size / 2 in
      let row = c mod size in
      let column = c / size in
      let v = _getf map (x-aid+row) (y-aid+column) in
      if fn v acc then _kernel_applicator size map fn (c+1) x y v else _kernel_applicator size map fn (c+1) x y acc


  let rec _loop ?(x=0) ?(y=0) i o fn =
    fn i o x y;
    if x < i.width-1 then _loop ~x:(x+1) ~y:y i o fn else
      if y < i.height-1 then _loop ~x:0 ~y:(y+1) i o fn else
        o

  let gray_of_point (i: _ image) (o: gray image) x y=
    match i.pixels with
    | `RGBA (rr, gg, bb, _) | `RGB (rr, gg, bb)  ->  pix_zip_3_to_1 rr gg bb (match o.pixels with | `GRAY (gg, _) -> gg) lums_of_rgb x y
    | `GRAY _ -> failwith "Not implememnted."

  let gray_of_image (i: _ image) =
    let o = create_gray i.width i.height in
    let t = _loop i o gray_of_point in t
  end