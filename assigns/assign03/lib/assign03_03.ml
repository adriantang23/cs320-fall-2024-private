type tree = 
| Leaf of int
| Node of tree list

let rec collapse h t =
  match t with
  | Leaf _ -> t
  | Node [] -> t
  | Node children ->
    if h = 1 then
      let rec collapseTerminals acc lst =
        match lst with
        | [] -> acc
        | hd :: tl ->
            let acc' = 
              match hd with
              | Leaf _ -> acc @ [hd]
              | Node [] -> acc @ [hd]
              | Node _ -> acc @ collapseTerminals [] (match hd with Node ch -> ch | _ -> [])
            in collapseTerminals acc' tl
      in Node (collapseTerminals [] children)
    else
      let rec collapseChildren acc lst =
        match lst with
        | [] -> acc
        | hd :: tl ->
            let acc' = acc @ [collapse (h - 1) hd]
            in
            collapseChildren acc' tl
      in Node (collapseChildren [] children)
