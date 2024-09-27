let sign x = if x > 0 then 1 else if x < 0 then -1 else 0
let group l =
  let rec grouper final current prev_sign lst =
    match lst with
    | [] -> 
        if current = [] then Some (List.rev final)
        else Some (List.rev (List.rev current :: final))
    | 0 :: tail ->
        if current = [] || prev_sign = 0 then None
        else 
            let next_sign =
            match tail with
                | head :: _ -> sign head
                | [] -> 0
            in if next_sign = (prev_sign * -1) then grouper (List.rev current :: final) [] 0 tail
            else None
    | head :: tail ->
        let current_sign = sign head in
        if current_sign = 0 then None
        else if prev_sign = 0 then
          grouper final [head] current_sign tail
        else if current_sign = prev_sign then
          grouper final (head :: current) prev_sign tail
        else None
  in
  grouper [] [] 0 l