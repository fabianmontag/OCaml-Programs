module IntMap = Map.Make(Int);;

let a = IntMap.empty;;
let b = IntMap.add 1 true a;;
IntMap.mem 1 b;;

let rec iter f k n = if n <= 0 then k else iter f (f k) (n-1);;

let nqueens n =
  let rec backtrack i q posDiagMap negDiagMap vertMap =
    if q = 0 then (true, [])
    else let (_, a, b) = (iter (fun k ->
      match k with
      | (j, true, x) -> (j+1, true, x)
      | (j, false, _) when
        (not (IntMap.mem j vertMap)) &&
        (not (IntMap.mem (i+j) posDiagMap)) &&
        (not (IntMap.mem (i-j) negDiagMap)) -> let (a, b) = 
          backtrack (i+1) (q-1)
          (IntMap.add (i+j) true posDiagMap)  
          (IntMap.add (i-j) true negDiagMap)  
          (IntMap.add j true vertMap) 
        in (j+1, a, (i, j) :: b)
      | (j, false, x) -> (j+1, false, x)
    ) (0, false, []) n) in (a, b)
  in backtrack 0 n IntMap.empty IntMap.empty IntMap.empty;;
  
nqueens 8;;