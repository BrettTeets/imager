module Point = struct
  
  type t = int * int

  let make x y = ((x, y):t)
  let x point = fst (point:t) 

  let y point = snd (point:t)
  
end