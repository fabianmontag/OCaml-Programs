let map = [[0; 0; 0; 0; 1; 1];
           [0; 1; 1; 0; 1; 0];
           [0; 1; 0; 0; 0; 0];
           [0; 0; 1; 1; 1; 0];
           [1; 1; 0; 0; 1; 0];
           [1; 0; 1; 0; 1; 1];
           [1; 0; 1; 0; 1; 1]];;

(* get length of list *)
let rec length l =
  match l with
  | [] -> 0
  | x :: l -> 1 + length l;;

(* get mode at index of list *)
let getAtIndex baseCVal l i =
  let rec getAtIndex l i =
    match l with
    | [] -> baseCVal
    | x :: l -> if i = 0 then x else getAtIndex l (i-1)
  in getAtIndex l i;;
  
let getAtIndex1D = getAtIndex (-1);;
let getAtIndex2D = getAtIndex [];;

(* check if node has been visited, using list as substitute for map *)
let rec inVisited i j v =
  match v with
  | [] -> false
  | x :: v -> fst x = i && snd x = j || inVisited i j v;;

(* function to count number of islands *)
(* approach: we go over every node, if water we ignore,
otherise, we do dfs to cover all 4-dir land mass and add nodes
to global visited to prevent double counting of land mass, we also
need to keep track of local nodes to prevent looping of current dfs*)
let countIslands map =
  let n = length (getAtIndex2D map 0) in 
  let m = length map in
  
  (* go over every node *)
  let rec step i gv =
    if i = n*m then 0
    else 
    if (getAtIndex1D (getAtIndex2D map (i / n)) (i mod n)) = 0 then 0 + step (i+1) gv else (* if water, skip *)
    let rec dfs i j lv =
      if i < 0 || j < 0 || i > m-1 || j > n-1 then (true, []) (* out of bounds *)
      else if ((getAtIndex1D (getAtIndex2D map i) j) = 0) then (true, []) (* if water, skip *)
      else if inVisited i j lv then (true, []) (* check local visited nodes, nodes visited in current dfs *)
      else if inVisited i j gv then (false, []) (* check global visited nodes, nodes visited in past dfs's *)
      (* we append nodes to local visited nodes to keep track of visited nodes while doing dfs
      top-down, and build the portion of local nodes to append to global nodes bottom-up*)
      else let lvu = lv @ [i, j] in
        let left = dfs (i+1) j lvu in let lvu = (lvu @ snd left) in 
        let bottom = dfs i (j+1) lvu in let lvu = (lvu @ snd bottom) in 
        let right = dfs (i-1) j lvu in let lvu = (lvu @ snd right) in 
        let top = dfs i (j-1) lvu in
      (fst left && fst bottom && fst right && fst top, [(i, j)] @ snd left @ snd bottom @ snd right @ snd top)
    in let r = dfs (i / n) (i mod n) [] in (if (fst r) then 1 else 0) + step (i+1) (gv @ snd r)
  in step 0 [];;

countIslands map;;
