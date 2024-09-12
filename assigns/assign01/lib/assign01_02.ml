let is_prime n =
    let rec check y =
        if y * y > n then true
        else if n mod y = 0 then false
        else check (y+1)
    in 
    if n < 2 then false
    else check 2

let nth_prime n =
    let rec find_n x y =
        if is_prime x then
        if y = n then x
        else find_n (x+1) (y+1)
    else
        find_n (x+1) y
    in find_n 2 0

    