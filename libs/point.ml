module Point = struct
  
  type t = int * int

  let make x y = ((x, y):t)
  let x point = fst (point:t) 

  let y point = snd (point:t)

  let add (a:t) (b:t) = (((fst a + fst b), (snd a + snd b)):t)

  let sub (a:t) (b:t) = (((fst b - fst a), (snd b - snd a)):t)

  let equal (a:t) (b:t) = (fst a = fst b) && (snd b = snd a)

  let rugged_equal (a:t option) (b:t) = if Option.is_none a then false else equal (Option.get a) b
  
end