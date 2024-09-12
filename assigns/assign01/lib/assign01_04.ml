open Assign01_02

let to_string n =
    let rec build n first = 
        if n = 1 then
            if first then "[]" else "]"
        else 
            let rec prime i =
                let p = nth_prime i in
                if n mod p = 0 then (p, i)
                else prime (i + 1)
            in
            let (p,i) = prime 1 in
            let format = 
                if first then "[" ^ string_of_int i 
                else "; " ^ string_of_int i in format ^ build (n/p) false
    in
    build n true


