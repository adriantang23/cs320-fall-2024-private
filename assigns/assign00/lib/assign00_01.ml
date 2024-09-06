let sqrt n = 
    let rec find k = 
        if k * k >= n then k
        else find (k+1)
    in find 0

