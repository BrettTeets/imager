module Contours = struct
  (**An implementation of suzuki's boundry detection algorithm.*)

  (*NBD of frame is 1.
    Before NBD is every used it is incremented.

    LNBD is last NBD

    Scan left to right until you find object pixel.
                              x-1 | x
        outer border of type 0    | 1
                              x   | x+1
        inner border of type >=1  | 0

    Everytime we start a new row reset LNBD to 1

    STEP 1:
        if its an outer border, 
            increment NBD
            set i2, j2 as i, j-1
        else if its a hole border
            increment NBD
            set i2, j2 as i, j+1 
            if fij > 1 then LNBD = fij
        else   
            skip to step three

    STEP 2:
        //trace border
        1) starting from i2, j2 (Dont include) look clockwise around i j,
            find nonzero pixel and denote it i1 j1
            if no nonzero pixels are found,
                set fij = -NBD and go to step 4.
        2) set i2, j2 = i1, j1 and i3, j3 = i, j
        3) starting from i2, j2, (Dont include) in counterclockwise order around i3, j3
            find first nonzero pixel and set it to i4, j4
        4) Change i3, j3 to
            if i3, j3+1 is 0, then set i3, j3 to -NBD
            if i3, j3+1 is >0 AND i3, j3 = 1 then set i3, j3 to NBD
            otherwise do not change pixel value.
        5) if in step 2.3 we return to the starting point i4, j4 = i j AND i3, j3 = i1, j1 go to step 3
            Otherwise set i2, j2 = i3, j3 and i3,j3 to i4,j4 and repeat step 2.3

    STEP 3:
        if fij != 1 then set LNBD = fij
            start scanning from the next pixel i, j+1, stopping when we reach the bottom right corner.



    OPENCV
        opencv supports a flat list of parent structures as opposed to a heirachy
        opencv supports simiplied chain for more compact storage.

    https://medium.com/ntust-aivc/digital-image-processing-contour-usage-77ab76918358
    https://docs.opencv.org/3.4/d4/d73/tutorial_py_contours_begin.html
    https://juliaimages.org/v0.22/democards/examples/contours/contour_detection/ 
        I have an annoted copy of this in my python folder.
    https://sdm.lbl.gov/~kewu/ps/LBNL-56864.pdf
*)

(*STEP 2: 
(Ediit)
i , j  = Init Point (One of our starting values.)
i1, j1 = Anchor Point
i3, j3 = Pivot Point
i2, j2 = Prev Point (One of our starting values.)
i4, j4 = next Point
    //trace border
    //Orentation.
    1) Look around Init-P in a clockwise motion from but not including Prev-p
        find nonzero pixel and denote it Anchor-P
        if no nonzero pixels are found,
            set Init-P = -NBD and go to step 4.
    2) set Prev-P = Anchor-P and Pivot-P = Init-P
    //Loop
    3) Look around Pivot-P in a counterclockwise order from but not including Prev-P
        find first nonzero pixel and set it to Next-P
    4) Setting the Pivot-P value
        if Pivot+east is 0, 
            then set Pivot-P to -NBD
        if Pivot+east is >0 AND Pivot-P = 1 
            then set Pivot-P to NBD
        otherwise do not change pixel value.
    5) if in step 2.3 we return to the starting point Next-P = Init-P AND Pivot-P = Anchor-P go to step 3
        Otherwise set Prev-P = Pivot-P and Pivot-P to Next-P and repeat step 2.3*)


(*I dont know if it helps but one particular insight I had was this is not a context-free problem, we are not
  finding the contours in isolation with this alogrithm we are finding them in relation to other contours.*)
  open Mappy.Mappy
  open Point

  (*This is the data structure we are returning. It organizes everything into a hiearchy.*)
  type contour =
  | Empty
  | Node of {next : contour ; prev : contour ; parent : contour ; child : contour ; points : Point.t list}


  let _extract_gray x _ = x
  let _extract_theta _ y = y
  let _extract_both x y = x, y 

  (*Direction used for moving about a 8- neighborhood of a pixel.*)
  type direction =
    | N (*-y*)
    | NE (*+x, -y*)
    | E (*+x*)
    | SE (*+x +y*)
    | S (*+y*)
    | SW (*-x +y*)
    | W (*-x*)
    | NW (*-x -y*)


  let north = Point.make 0 ~-1
  let northwest = Point.make ~-1 ~-1
  let west = Point.make ~-1 0
  let southwest = Point.make ~-1 1
  let south = Point.make 0 1
  let southeast = Point.make 1 1
  let east = Point.make 1 0
  let northeast = Point.make 1 ~-1

  let clockwise center direction =
    (*returns the coordinates to the next clockwise pixel based on your center and previous direction.*)
    match direction with
    | N -> Point.add center north, NE
    | NE -> Point.add center northeast, E
    | E -> Point.add center east, SE
    | SE -> Point.add center southeast, S
    | S -> Point.add center south, SW
    | SW -> Point.add center southwest, W
    | W -> Point.add center west, NW
    | NW -> Point.add center northwest, N

  let counterclockwise center direction = 
    match direction with
    | N -> Point.add center north, NW
    | NW -> Point.add center northwest, W
    | W -> Point.add center west, SW
    | SW -> Point.add center southwest, S
    | S -> Point.add center south, SE
    | SE -> Point.add center southeast, E
    | E -> Point.add center east, NE
    | NE -> Point.add center northeast, N

  let turn_counterclockwise direction = 
    match direction with
    | N  -> NW
    | NW -> W
    | W  -> SW
    | SW -> S
    | S  -> SE
    | SE -> E
    | E  -> NE
    | NE -> N

  let from_a_to_b (a:Point.t) (b:Point.t) =
    let c = Point.sub a b in
    match c with
    | (0, (-1)) -> N
    | (1, (-1)) -> NE
    | (1, 0) -> E
    | (1, 1) -> SE
    | (0, 1) -> S
    | ((-1), 1) -> SW
    | ((-1), 0) -> W
    | ((-1), (-1)) -> NW
    | _ -> failwith "a and b are not adjacent."
  ;;

  (*1) starting from i2, j2 (Dont include) look clockwise around i j,
            find nonzero pixel and denote it i1 j1
            if no nonzero pixels are found,
                set fij = -NBD and go to step 4.*)

  (*This is actually some sort of orientation step.*)
  (*I dont think fi2j2 should be included in this? Need to see with inner contour*)
  let rec clockwise_search img fij fi2j2 =
    let start_dir = from_a_to_b fij fi2j2 in
    _clockwise_search img fij start_dir start_dir
  and _clockwise_search img fij initial_dir dir_to_try =
    let current, next_dir = clockwise fij dir_to_try in (*gets the point in that direction, and returns the next direction to try.*)
    let neighbor = read_gray img _extract_gray (fst current) (snd current) in
    if neighbor != 0. then Some current else
      if next_dir == initial_dir then None else
        _clockwise_search img fij initial_dir dir_to_try
  ;;

(*STEP 2: 
(Ediit)
i , j  = Init Point (One of our starting values.)
i1, j1 = Anchor Point
i3, j3 = Pivot Point
i2, j2 = Prev Point (One of our starting values.)
i4, j4 = next Point
    //trace border
    //Orentation.
    1) Look around Init-P in a clockwise motion from but not including Prev-p
        find nonzero pixel and denote it Anchor-P
        if no nonzero pixels are found,
            set Init-P = -NBD and go to step 4.
    2) set Prev-P = Anchor-P and Pivot-P = Init-P
    //Loop
    3) Look around Pivot-P in a counterclockwise order from but not including Prev-P
        find first nonzero pixel and set it to Next-P
    4) Setting the Pivot-P value
        if Pivot+east is 0, 
            then set Pivot-P to -NBD
        if Pivot+east is >0 AND Pivot-P = 1 
            then set Pivot-P to NBD
        otherwise do not change pixel value.
    5) if in step 2.3 we return to the starting point Next-P = Init-P AND Pivot-P = Anchor-P go to step 3
        Otherwise set Prev-P = Pivot-P and Pivot-P to Next-P and repeat step 2.3*)
  let rec find_next_point img prev pivot =
    let dir = from_a_to_b pivot prev |> turn_counterclockwise in
    _counterclockwise_search img pivot dir dir
  and _counterclockwise_search img pivot dir init_dir =
    let current, next_dir = counterclockwise pivot dir in
    let neighbor = read_gray img _extract_gray (fst current) (snd current) in
    if neighbor != 0. then Some current else
      if next_dir == init_dir then None else
        _counterclockwise_search img pivot next_dir init_dir

  ;;

    
  

(*STEP 2: 
(Ediit)
i , j  = Init Point (One of our starting values.)
i1, j1 = Anchor Point
i3, j3 = Pivot Point
i2, j2 = Prev Point (One of our starting values.)
i4, j4 = next Point
    //trace border
    //Orentation.
    1) Look around Init-P in a clockwise motion from but not including Prev-p
        find nonzero pixel and denote it Anchor-P
        if no nonzero pixels are found,
            set Init-P = -NBD and go to step 4.
    2) set Prev-P = Anchor-P and Pivot-P = Init-P
    //Loop
    3) Look around Pivot-P in a counterclockwise order from but not including Prev-P
        find first nonzero pixel and set it to Next-P
    4) Setting the Pivot-P value
        if Pivot+east is 0, 
            then set Pivot-P to -NBD
        if Pivot+east is >0 AND Pivot-P = 1 
            then set Pivot-P to NBD
        otherwise do not change pixel value.
    5) if in step 2.3 we return to the starting point Next-P = Init-P AND Pivot-P = Anchor-P go to step 3
        Otherwise set Prev-P = Pivot-P and Pivot-P to Next-P and repeat step 2.3*)
  let rec neighborhood_search img init prev nbd=
    let anchor = clockwise_search img init prev in
    if Option.is_none anchor then write_gray img (fst init) (snd init) (~-.nbd) 0. else
      let anchor = Option.get anchor in
      let prev, pivot = anchor, init in
      let _(*border*) = _inner img prev pivot nbd init anchor [] in 
    let value = read_gray img _extract_gray (fst init) (snd init) in
    if value != 1. then (*lnbd = nbd*) () else ()
  and _inner img prev pivot nbd init anchor border =
    ((let border = pivot :: border in
    let next = find_next_point img prev pivot in
    (*This is part of the marking.*)
    (let neighbor_value = read_gray img _extract_gray (fst pivot) (snd pivot + 1) in
    if neighbor_value = 0. then write_gray img (fst pivot) (snd pivot) ~-.nbd 0. else
      let pivot_value = read_gray img _extract_gray  (fst pivot) (snd pivot) in
      if neighbor_value > 0. && pivot_value = 1. then  (write_gray img (fst pivot) (snd pivot) nbd 0.; ) else ());
    (*This is the continuation of the loop with its base case and recursive case.*)
    if Point.rugged_equal next init && Point.equal pivot anchor then border else
      _inner img pivot (Option.get next) nbd init anchor border):Point.t list)




      
  ;;






    



  (* type cell_response = { rc : Point.t ; d : direction ; s : Point.t option }

  (*The core *)
  let rec find_contours (img:gray image) =
    (*x and y start at 1 to avoid the outer edge of the image. nbd and lnbd start at 1 because the algo says so.*)
    let nbd : int ref = {contents = 1} in
    let lnbd : int ref = {contents = 1} in
    _find_contours img 1 1 nbd lnbd

  and _find_contours img (x: int) (y: int) (nbd: int ref) lnbd =
    let this, prev, next = (read_gray img _extract_gray x y), (read_gray img _extract_gray x (y-1)), (read_gray img _extract_gray x (y+1)) in
    if this = 1. && prev = 0. then (
      nbd := (!nbd + 1);
      let direction = 2 in
      let contour = _follow_border img x y direction nbd in
        ()
    
       ) else
    if this >= 1 && next = 0 then 
      () else
        ()
    
    ; (*Main unit of work*)
    if x < img.width-2 then _find_contours img (x+1) y nbd lnbd else
      if y < img.height-2 then _find_contours img 1 (y+1) nbd lnbd else
        ()
  and _follow_border img x y (dir:direction) nbd =
    let curr: Point.t = (Point.make x y) in
    let exam : Point.t = Point.make x (y-1) in
    if (read_gray img _extract_gray x y) = 0. then 
    
    let exam2, dir, save = _next_cell curr dir in
    if Option.is_some save then ()
    if  
    ()
    
    else 2
  and _next_cell p (dir:direction) =
    let x = Point.x p in
    let y = Point.y p in
    match dir with
    | Right -> Point.make (x-1) y  , Up    , Some (Point.make x (y+1)) 
    | Up    -> Point.make x   (y-1), Left  , None          
    | Left  -> Point.make (x+1) y  , Down  , None          
    | Down  -> Point.make x   (y+1), Right , None          

  ;; *)

end