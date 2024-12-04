type bst = Empty | Node of int * bst * bst;;

let empty = Empty;;

let rec contains n bst = match bst with
  | Empty -> false
  | (Node (v, l, r)) -> v = n || contains n l || contains n r;;

let rec insert n bst = match bst with
  | Empty -> Node (n, Empty, Empty)
  | (Node (v, l, r)) -> 
    if v = n then Node (v, l, r) else
    if v < n then Node (v, l, insert n r)
    else Node (v, insert n l, r);;
    
let rec whileDo f g k = if g k then whileDo f g (f k) else k;;
    
let getInOrderSucc bst = match bst with
  | Empty -> Empty
  | Node (_, _, Empty) -> Empty
  | Node (_, _, r) -> whileDo 
    (fun bst -> match bst with | Node (_, l, _) -> l | x -> x)
    (fun bst -> match bst with Empty | Node (_, Empty, _) -> false | _ -> true)
    r;;
    
getInOrderSucc (Node (10, Node (5, Empty, Empty), Node (8, Node (6, Empty, Empty), Empty)));;
 
let rec delete n bst = match bst with
  | Empty -> Empty
  | Node (v, l, r) -> if n > v then Node (v, l, delete n r)
    else if n < v then Node (v, delete n l, r)
    else match l, r with
    | Empty, Empty -> Empty
    | l, Empty -> l
    | Empty, r -> r
    | _ -> let k = getInOrderSucc (Node (v, l, r)) in
           match k with
           | Empty -> l
           | (Node (v2, _, _)) -> Node (v2, l, delete v2 r);;
    
let bstT = Node (5, Node (4, Empty, Empty), Node (12, Empty, Empty));;
insert 8 bstT;;
contains 3 bstT;;

let rec depth bst = match bst with
  | Empty -> 0
  | (Node (v, l, r)) -> 1 + Int.max (depth l) (depth r);;
  
let rightRot bst = match bst with
  | Empty -> bst
  | (Node (v, Empty, _)) -> bst
  | (Node (v, Node(v2, l, r2), r)) ->
     Node (v2, l, Node (v, r2, r));;
     
let leftRot bst = match bst with
  | Empty -> bst
  | (Node (v, _, Empty)) -> bst
  | (Node (v, l, Node (v2, l2, r))) ->
    Node (v2, Node (v, l, l2), r);;

let rec insertAVL n bst = match bst with
  | Empty -> Node (n, Empty, Empty)
  | (Node (v, l, r)) -> 
    if v = n then Node (v, l, r) else
    if v < n then
      let r = insertAVL n r in
      let diff = (depth l)-(depth r) in
      match r with | Empty -> Node (v, l, r)
      | (Node (v2, _, _)) ->
      (* inserting on right side, possibly unbalanced *)
      (* Right Right *)
      if diff < (-1) && n > v2 then leftRot (Node (v, l, r))
      (* Right Left *)
      else if diff < (-1) && n < v2 then leftRot (Node (v, l, rightRot r))
      else (Node (v, l, r))
    else
      let l = insertAVL n l in
      let diff = (depth l)-(depth r) in
      match l with | Empty -> Node (v, l, r)
      | (Node (v2, _, _)) ->
      (* inserting on left side, possibly unbalanced *)
      (* Left Left *)
      if diff > 1 && n < v2 then rightRot (Node (v, l, r))
      (* Left Right *)
      else if diff > 1 && n > v2 then rightRot (Node (v, leftRot l, r))
      else (Node (v, l, r));;
      
let bstT = Node (5, Node (4, Node (3, Empty, Empty), Empty), Node (12, Node (9, Empty, Empty), Empty));;
insertAVL 11 bstT;;

let bstT2a = empty;;
let bstT2b = insertAVL 10 bstT2a;;
let bstT2c = insertAVL 20 bstT2b;;
let bstT2d = insertAVL 30 bstT2c;;
let bstT2e = insertAVL 40 bstT2d;;
let bstT2f = insertAVL 50 bstT2e;;
let bstT2g = insertAVL 60 bstT2f;;
let bstT2h = insertAVL 70 bstT2g;;

(*
      40
  20      60  
10  30  50  70
*)

let d1 = delete 40 bstT2h;;

(*
      50
  20      60  
10  30      70
*)

let getBalance bst = match bst with
  | Empty -> 0
  | Node (_, l, r) -> depth l - depth r;;

let balanceAVL bst = match bst with
  | Empty -> Empty
  | Node (v, l, r) ->
    let b = getBalance bst in
    let lb = getBalance l in
    let rb = getBalance r in
    (* Left Left *)
    if b > 1 && lb >= 0 then
      rightRot bst
    (* Left Right *)
    else if b > 1 && lb < 0 then
      leftRot (Node (v, l, rightRot r))
    (* Right Right *)
    else if b < -1 && rb <= 0 then
      leftRot bst
    (* Right Left *)
    else if b < -1 && rb > 0 then
      rightRot (Node (v, leftRot l, r))
    else Node (v, l, r);;
     
let rec deleteAVL n bst = match bst with
  | Empty -> Empty
  | Node (v, l, r) -> if n > v then balanceAVL (Node (v, l, deleteAVL n r))
    else if n < v then balanceAVL (Node (v, deleteAVL n l, r))
    else match l, r with
    | Empty, Empty -> Empty
    | l, Empty -> balanceAVL l
    | Empty, r -> balanceAVL r
    | _ -> let k = getInOrderSucc (Node (v, l, r)) in
           match k with
           | Empty -> l
           | (Node (v2, _, _)) -> balanceAVL (Node (v2, l, deleteAVL v2 r));;

let d1 = deleteAVL 70 (deleteAVL 60 (deleteAVL 40 bstT2h));;