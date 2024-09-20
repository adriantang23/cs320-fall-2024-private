type matrix = {
entries : float list list;
rows : int;
cols : int;
}

let mk_matrix entries (r,c) =
    let rec partition lst n =
        match lst with
        | [] -> []
        | _ -> List.take n lst :: partition (List.drop n lst) n
    in
    let final = partition entries c in
    {
        entries = final;
        rows = r;
        cols = c;
    }