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
  (* type contour =
  | Empty
  | Node of {next : contour ; prev : contour ; parent : contour ; child : contour ; points : Point.t list} *)


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

  let turn_clockwise direction = 
    match direction with
    | N  -> NE
    | NE -> E
    | E  -> SE
    | SE -> S
    | S  -> SW
    | SW -> W
    | W  -> NW
    | NW -> N

  let print_direction str direction = 
    match direction with
    | N  -> print_endline @@ str ^ "N"
    | NE -> print_endline @@ str ^ "NE"    
    | E  -> print_endline @@ str ^ "E"
    | SE -> print_endline @@ str ^ "SE"    
    | S  -> print_endline @@ str ^ "S"
    | SW -> print_endline @@ str ^ "SW"    
    | W  -> print_endline @@ str ^ "W"
    | NW -> print_endline @@ str ^ "NW"

  let from_a_to_b (a:Point.t) (b:Point.t) =
    let c = Point.sub a b in
    match c with
    | (0, (-1)) -> N
    | (1, (-1)) -> NE
    | (1, 0) ->  E
    | (1, 1) ->  SE
    | (0, 1) -> S
    | ((-1), 1) -> SW
    | ((-1), 0) -> W
    | ((-1), (-1)) -> NW
    | _ -> failwith "a and b are not adjacent."
  ;;

  let read_img = read_binary (fun x -> x)
  let is_outer prev curr = curr = 1 && prev = 0 
  let is_inner curr next = curr >= 1 && next = 0

  (*This is a generalized method for preform the clockwise and counterclockwise sweeps*)
  let rec _search fn img curr init_dir curr_dir =
    let current, next_dir = fn curr curr_dir in
    let value = read_img img (fst current) (snd current) in
    if value != 0 then Some current else
      if next_dir = init_dir then None else
        _search fn img curr init_dir next_dir

  let clockwise_search = _search clockwise
  let counterclockwise_search = _search counterclockwise

  (*This is the process of marking drawn out into its own function.*)
  let marking img pivot nbd = 
    (*This is gathering values for marking.*)
    let east_value = read_img img (fst pivot+1)(snd pivot) in
    let pivot_value = read_img img (fst pivot) (snd pivot) in
    (*This is part of marking.*)
     ( let mark_pivot n = write_binary n img (fst pivot) (snd pivot) in
      if east_value = 0 then (mark_pivot (~-nbd)) else
      if east_value > 0 && pivot_value = 1 then mark_pivot nbd else 
      ()
    )


  (**[find_anchor img curr prev] Find anchor is the first step in the algo. It will sweep in a clockwise direction and find the pixel 'behind' this one
      in the contour. So when we follow the contour and we see both our initial pixel and our anchor pixel we know we have looped around. Returns none 
      if it cant find a pixel.*)
  let find_anchor img curr prev =
    (*Do not check the direction to the previous pixel, we know that one is blank.*)
    let dir = from_a_to_b curr prev |> turn_clockwise in clockwise_search img curr dir dir


  (**[find_next_point img pivot prev] Find next point will return the next point in the contour if there is one by looking in a counter clockwise direction
    in front of the contour. The code handles the per pixel task of finding one point. crawl points uses it to crawl all of them. This will return none if 
    no contour is found. *)
  let find_next_point img pivot prev =
    (*Do not check the direction to the previous pixel, we know that one is filled and its behind us. We are crawling along the outer edge, staying on
    1-points and keeping 0-points to our right.*)
    let dir = from_a_to_b pivot prev |> turn_counterclockwise in
    counterclockwise_search img pivot dir dir


  (**[crawl_points img prev pivot nbd init anchor points] Crawl points handles actuall walking the length of the contour after we have have found the anchor.
     crawl_points halso preforms the task of marking the img so we can accuaretly build the heiarchy. With every recursive call pivot is moved to previous and
     next is moved to pivot. Slowly walking forward until we reach our starting point or find next point fails to find the next point. *)
  let rec crawl_points img prev pivot nbd init anchor points =
    let next = find_next_point img pivot prev in
    (*This preforms the generalized marking of this pixel. nbd or -nbd depending on the results.*)
    marking img pivot nbd;
    (*actually append our pivot to our points.*)
    let points = pivot :: points in
    (*THis is the loop with our base case and recursive case.*)
    if Option.is_none next then points else (*This is probably a line.*)
      let next = Option.get next in
      if Point.equal next init && Point.equal pivot anchor then points else (*This is the natural end of our contour.*)
        crawl_points img pivot next nbd init anchor points 


  (**[walk_contour img init prev nbd] walk contour sets up finding the anchor point, setting up init, previous and current, and then calls crawl points
  to progress around the object we found.*)
  let walk_contour img init prev nbd =
    let anchor = find_anchor img init prev in 
    if Option.is_none anchor then 
      (*Mark this spot and then return, I dont think I should be returning empty here though I will need to check.*)
      (*Walk contours should return a list of points, this says I could not find a anchor to start the search.*)
      (write_binary (~-nbd) img (fst init) (snd init); (*init ::*) [])
    else 
    (let anchor = Option.get anchor in
      let prev, pivot = anchor, init in
      let points = init :: [] in (*go ahead and push the first point on.*)
      crawl_points img prev pivot nbd init anchor points)


  let rec find_contour_list img =
    raster_scan img 1 1 [] 1
  and raster_scan img x y acc nbd =
    let previous = read_img img (x-1) y in
    let current = read_img img x y in
    let next = read_img img (x+1) y in
    let ll, nnbd = (if (is_outer previous current || is_inner current next) then 
                      _support img previous current next x y acc (nbd+1)
              else acc, nbd) in
    (if x < img.width-2 then  raster_scan img (x+1) y ll nnbd else
      if y < img.height-2 then raster_scan img 1 (y+1) ll nnbd else
        ll)
  and _support img previous current next x y acc nbd =
    if is_outer previous current then 
      (let curr = Point.make x y in
      let prev = Point.make (x-1) y in
      walk_contour img curr prev nbd :: acc, nbd)
    else if is_inner current next then
      (print_endline "Is this being called?";
      let curr = Point.make x y in
      let fore = Point.make (x+1) y in
      walk_contour img curr fore nbd :: acc, nbd)
    else acc, nbd
  ;;
  
  type contour =
| Empty
| Node of {next : contour ; prev : contour ; parent : contour ; child : contour ; points : Point.t list}

  (*This is not needed for the next step of camera calibration I think but it is a nice to have.
    My current idea is that when scanning the raster image you can treat the numbers like paranthesis
    to tell when you are in another contour so 8 should pair with -8 and if you find a hole contour
      while in 8 it is probably 8's inner edge. This idea could let you structure the tree data structure.
    Will need to do more work to figure out if I even should store it as a real tree or just have indexs into
      an array. I will say I dont like returning them seperately like openCV.- 3/30/25
      *)
  let rec find_contour_tree img =
    raster_scan img 1 1 Empty 1
  and raster_scan img x y acc nbd =
    let output, nnbd = (_support img x y acc nbd) in
    (if x < img.width-2 then raster_scan img (x+1) y output nnbd else
      if y < img.width-2 then raster_scan img 1 (y+1) output nnbd else
        output)
  and _support img x y acc nbd =
    let previous = read_img img (x-1) y in
    let current = read_img img x y in
    let next = read_img img (x+1) y in
    if (is_outer previous current || is_inner current next) then (
      if is_outer previous current then 
        (let curr = Point.make x y in
        let prev = Point.make (x-1) y in
        (build_contour (walk_contour img curr prev nbd) acc, nbd))
      else if is_inner current next then
        (print_endline "Is this being called?";
        let curr = Point.make x y in
        let fore = Point.make (x+1) y in
        (build_contour (walk_contour img curr fore nbd) acc, nbd))
      else acc, nbd
    )
    else acc, nbd
  and build_contour points acc =
    match acc with
    | Empty -> Node {next = Empty ; prev = Empty ; parent = Empty ; child = Empty ; points }
    | Node _ -> Node {next = Empty ; prev = Empty ; parent = Empty ; child = Empty ; points }

  ;;




end