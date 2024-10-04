let calc_lifespan f start pred =
    let rec survives input count =
        if pred input then count
        else survives (f input) (count + 1)
    in
    survives start 0

let find_max lst =
    match lst with
    | [] -> None
    | x :: xs -> Some (List.fold_left max x xs)

let find_winner winner lst = 
    let rec trav lst ind last_found = 
        match lst with
        | [] -> last_found
        | x :: xs -> 
            if x = winner then trav xs (ind+1) (Some ind)
            else trav xs (ind+1) last_found
    in
    trav lst 0 None


let last_function_standing funcs start pred = 
    match funcs with
    | [] -> None
    | _ -> 
        let lifespans = List.map (fun f -> calc_lifespan f start pred) funcs in 
        match find_max lifespans with
        | None -> None
        | Some max_found ->
            let dupe span lst = 
                List.fold_left (fun count x -> if x = span then count + 1 else count) 0 lst
            in if dupe max_found lifespans > 1 then None
            else 
                match find_winner max_found lifespans with
                | None -> None
                | Some index -> Some (List.nth funcs index)
