(* isomorphic tree test *)
let rec iso (L (l, ls)) (L (l', ls')) =
    l = l' &&
    length ls = length ls' &&
    foldl (fun st (b', ls') ->
        foldl (fun st' (b, ls') ->
            if b then (b, ls' @ [st']) else
            if iso st st' then (true, ls')
            else (false, ls' @ [st'])
        ) ls' (false, []) 
        |> (fun (b, l) -> (b && b', l))
    ) ls (true, ls') |> fst
