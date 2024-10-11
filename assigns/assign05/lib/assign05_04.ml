type set_info = {
ind : int -> bool;
mn : int;
mx : int;
}

module ListSet = struct
    type t = int list
    let mem x set = List.mem x set
    let empty = []
    let singleton x = [x]

    let card set = 
        let rec new_elem l seen =
            match l with
            | [] -> seen
            | h :: t -> 
                if List.mem h seen then new_elem t seen
                else new_elem t (h :: seen)
        in List.length (new_elem set [])

    let union s1 s2 =
        let rec merge l1 l2 =
        match l1, l2 with
        | [], l | l, [] -> l
        | h1 :: t1, h2 :: t2 ->
            if h1 < h2 then h1 :: merge t1 l2
            else if h1 > h2 then h2 :: merge l1 t2
            else h1 :: merge t1 t2
        in
        merge s1 s2
end


module FuncSet = struct
    type t = set_info
    let mem x set = set.ind x
    let empty = { ind = (fun _ -> false); mn = max_int; mx = min_int}
    let singleton x = { ind = (fun y -> y = x); mn = x; mx = x}

    let card s =
        let rec count acc i =
            if i > s.mx then acc
            else if s.ind i then count (acc+1) (i+1)
            else count acc (i+1)
        in
        if s.mx < s.mn then 0 else count 0 s.mn

    let union s1 s2 = 
        if s1.mx < s1.mn then s2
        else if s2.mx < s2.mn then s1
        else
          { ind = (fun x -> s1.ind x || s2.ind x);
            mn = min s1.mn s2.mn;
            mx = max s1.mx s2.mx }
end