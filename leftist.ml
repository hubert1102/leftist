type 'a queue = 
    | Node of 'a queue * 'a * 'a queue * int
    | Leaf
;;

let empty = Leaf
;;

exception Empty
;;

let is_empty a =
    match a with
    | Leaf -> true
    | _ -> false
;;
let height q =
    match q with
    |Leaf -> 0
    |Node(_, _, _, h) -> h
;;
let rec join q1 q2 =
    match q1, q2 with
        | Leaf, q -> q2
        | q, Leaf -> q1
        | Node(q1l, p1, q1r, h1), Node(q2l, p2, q2r, h2) ->
            if (p1 > p2)
                then join q2 q1
                else let rjoin = join q1r q2
                    in let hx = height rjoin
                        in let hy = height q1l
                            in if hy >= hx 
                                then Node(rjoin, p1, q1l, hy+1)
                                else Node(q1l, p1, rjoin, hx+1)
;;

let add a q = 
    join q (Node(Leaf, a, Leaf, 1))
;;

let delete_min q = 
    match q with
    | Leaf -> raise Empty
    | Node (ql, p, qr, _) -> (p, join ql qr)
;;
