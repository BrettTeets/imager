module type Kernel = sig
open Mappy.Mappy
  val apply_kernel : int -> float list -> gray image -> int -> int -> float
  val compare_kernel : int -> (float -> float -> bool) -> gray image -> float -> int -> int -> float
end
module Kernel : Kernel = struct
  open Mappy.Mappy

  (*API for calling into kernels. 
      - Select a size of kernel, it needs to be odd number. (This only varies per module, they can be set up before hand.)
      - Provide a kernel to work over, needs to have enough elements. (This only varies per module, they can be set up before hand.)
      - center point for the kernel to work over. (This will vary with every call)
      - And an image to apply the kernel too. (This will vary with every call)
      
    My kernels by design do not validate data. They need to be placed into validators if that is a feature you want.*)


  let get_relative_position size steps =
    (*This gets the relative position of x y based on how many steps have been taken so far. *)
    let row = steps mod size in
    let column = steps / size in
    row, column


  (** [apply_kernel_grayscale size image kernel x y] size of the kernel is 3 5 or 7, image must be grayscale, kernel must be of floats, x and y is the center point.
    returns the the result of apply the kernel to the given image. Does not overwrite. This applies the kernel to each pixel and sums the results.*)
  let rec apply_kernel size kernel (image:gray image) x y = 
    let offset = size / 2 in
    match image.pixels with
    | `GRAY (gg, _) -> _kernel_applicator size offset gg kernel 0 x y
    and _kernel_applicator size offset m kernel c x y =
      if c = size*size then 0. else (*Base caes.*)
      let row, column = get_relative_position size c in
      (_apply m kernel (x-offset+row) (y-offset+column) c) +. (_kernel_applicator size offset m kernel (c+1) x y)
    and _apply (m:mapf) k x y i =    
      (_getf m x y *. List.nth k i)

    (** [compare_kernel size fn image start x y] size of kenrnel, 3 5 or 7. the function to compare the results and return a value, a grayscale image, and the 
    initial value for the comparison. x and y coordinates of the center. This does not overwrite the image it just returns the value of best match for that comparitor. *)
    let rec compare_kernel size fn (image:gray image) start x y = 
    let offset = size / 2 in
    match image.pixels with
    | `GRAY (gg, _) -> _kernel_applicator size offset gg fn 0 x y start
    and _kernel_applicator size offset map fn c x y acc =
      if c = size*size then acc else (*Base caes.*)
      let row, column = get_relative_position size c in
      let v = _getf map (x-offset+row) (y-offset+column) in
      if fn v acc then _kernel_applicator size offset map fn (c+1) x y v else _kernel_applicator size offset map fn (c+1) x y acc
  
end