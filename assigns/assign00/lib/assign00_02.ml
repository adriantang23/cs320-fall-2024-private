let is_prime n =
    let rec check y =
        if y * y > n then true
        else if n mod y = 0 then false
        else check (y+1)
    in 
    if n < 2 then false
    else check 2
