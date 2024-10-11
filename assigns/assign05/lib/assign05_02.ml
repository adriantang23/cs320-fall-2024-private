type 'a tree = 
| Leaf
| Node of 'a * 'a tree * 'a tree

let sum_tr t = 
    let rec fold xs acc =
        match xs with
        | [] -> acc
        | Leaf :: rest -> fold rest acc
        | Node (x,l,r) :: rest ->
            fold (l::r::rest) (acc + x)
    in
    fold [t] 0
