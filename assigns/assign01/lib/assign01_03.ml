open Assign01_02

let rec intExpo base exp =
    if exp = 0 then 1
    else base * intExpo base (exp-1)

let nth s i =
    let rec exponent p s =
        if s mod p <> 0 then 0
        else 1 + exponent p (s/p)
    in 
    (* recursive function to update s through prime factorization using exponent helper *)
    let rec counter s ii = 
        if ii = i then exponent (nth_prime ii) s
        else 
            let p = nth_prime ii in 
            let expo = exponent p s in
            let update = s / (intExpo p expo) in
            counter update (ii + 1)
    in
    counter s 0

