open Utils

let parse toks = 
    let rec parse_helper toks stack =
        match toks with
        | [] -> (
            match stack with
            | [expr] -> Some expr
            | _ -> None)
        | tok :: rest ->
            match tok with
            | TNum n -> parse_helper rest (Num n :: stack)
            | TAdd -> (match stack with
                | e2 :: e1 ::tail -> parse_helper rest (Add (e1,e2) :: tail)
                | _ -> None)
            | TLt -> (match stack with
                | e2 :: e1 :: tail -> parse_helper rest (Lt (e1,e2)::tail)
                | _ -> None)
            |TIte -> (match stack with
            | e3 :: e2 :: e1 :: tail -> parse_helper rest (Ite (e1,e2,e3):: tail)
            | _ -> None)
    in parse_helper toks []
