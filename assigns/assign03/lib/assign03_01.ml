let mk_unique_keys alst = 
    let rec tracker alst result = 
        match alst with
        | [] -> result
        | (key, value) :: rest ->
            let updated = try
                let currentVal = List.assoc key result in
                    (key, currentVal + value) :: List.remove_assoc key result
                with
                | Not_found -> (key,value) :: result
            in tracker rest updated
    in
    tracker alst []