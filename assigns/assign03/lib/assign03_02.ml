(*let gen_fib l k = 
    let len = List.length l in

    let rec sums lst n = 
        match lst, n with 
        | [],_ -> 0
        | _, 0 -> 0
        | x :: y, n -> x + sums y (n-1)
    in
    
    let rec fib seq n =
        if n >= k then 
            match seq with
            | x :: _ -> x
            | [] -> 0
        else
            let temp_term = sums seq len in
            fib (temp_term :: (if List.length seq < len then seq 
                else match seq with
                | [] -> []
                | _ :: tl -> tl)) (n+1)
    in
    if k < len then List.nth l k
    else fib (List.rev l) len*)

let gen_fib l k =
    let len = List.length l in
        if k < len then List.nth l k
        else 
            let rec fib l n = 
                match l, n with 
                | [], _ -> 0
                | _, 0 -> 0
                | x :: xtl, n -> x + fib xtl (n-1)
        in
        let rec sums current n =
            if n = k then fib current len
            else
                let total = fib current len in
                sums (total :: current) (n+1)
    in
    sums (List.rev l) len