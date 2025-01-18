type tree = T of tree list;;

let rec iter f n k = if n <= 0 then k else iter f (n-1) (f k);;
let rec foldl f l b = match l with | [] -> b | x :: l -> foldl f l (f x b);;
let map f l = foldl (fun a b -> f a :: b) l [];;
let filter_rev l p = foldl (fun a b -> if p a then a :: b else b) l [];;

let rec pl l = match l with
    | [] -> [[]]
    | l' :: l ->
        let cs = pl l in
        foldl (fun a r ->
            map (fun b -> a :: b) cs @ r
        ) l' [];;

(* generate all possible rose trees of size n *)
let rec cs n =
    if n <= 1 then [T []]
    else
        let p = iter (fun (i, acc) -> (i-1, (i, cs i) :: acc)) (n-1) (n-1, []) |> snd
        in let gcombs l =
            let rec gcombs l n = (
                if n <= 0 then [] else
                match l with
                | _ -> 
                    foldl (fun a res ->
                        let c = gcombs l (n-1) in
                        [a] :: res @ map (fun b -> a :: b) c
                    ) l []
            )
            in gcombs l (foldl (fun _ b -> b + 1) l 0)
        in let addsTo l = foldl (fun (c, _) b -> b + c) l 0 = (n-1)
        in let combs = filter_rev (gcombs p) addsTo
        in foldl (fun l b ->
            let t = pl (foldl (fun (_, ts) b -> ts :: b) l [])
            in foldl (fun a b -> T a :: b) t b
        ) combs [];;
