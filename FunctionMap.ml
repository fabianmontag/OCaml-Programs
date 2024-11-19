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
    | Some v -> fun k2 -> if k2 = kn then Some v else (delete map k) k2;;

let map = empty ();;
map;;
let map = update map 5 1337;;
lookup map 5;;
let map = update map 42 101;;
lookup map 42;;
lookup map 101;;
delete map 42;;
let map = update map 42 101;;
let map = delete map 42;;
lookup map 42;;
lookup map 5;;
let map = rename map 5 102;;
lookup map 5;;
lookup map 102;;