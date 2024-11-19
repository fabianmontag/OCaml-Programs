let empty () = fun k -> None;;

let update map k v =
    fun k2 -> if k2 = k then Some v else map k2;;
    
let lookup map k = map k;;

let delete map k =
    fun k2 -> if k2 = k then None else map k2;;
    
let rename map k kn =
    let v = map k in
    match v with
    | None -> map
    | _ -> fun k2 -> if k2 = kn then v else (delete map k) k2;;

(* testing *)
let map2 = empty ();;
map2 1337;;
let map3 = update map2 1337 10;;
map3 1337;;
let map4 = update map3 8897 20;;
map4 1337;;
map4 8897;;
map4 42;;
let map4 = update map3 8897 42;;
map4 1337;;
map4 8897;;
map4 42;;
let map5 = delete map4 1337;;
map5 1337;;
map5 8897;;
map5 42;;
let map6 = rename map5 8897 1337;;
map6 1337;;
map6 8897;;
map6 42;;
let map7 = rename map6 42 1337;;
map7 1337;;
map7 8897;;
map7 42;;
