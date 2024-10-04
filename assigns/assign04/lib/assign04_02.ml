type expr = 
| True
| False
| Num of int
| Or of expr * expr
| Add of expr * expr
| IfThenElse of expr * expr * expr
type ty = 
| Int
| Bool

let rec type_of e =
    match e with 
    | True | False -> Some Bool
    | Num _ -> Some Int
    | Add (n1,n2) ->
        (match (type_of n1, type_of n2) with
        | (Some Int, Some Int) -> Some Int
        | _ -> None)
    | Or (e1,e2) ->
        (match (type_of e1, type_of e2) with
        | (Some Bool, Some Bool) -> Some Bool
        | _ -> None)
    | IfThenElse (b,e1,e2) ->
        (match (type_of b, type_of e1, type_of e2) with
        | (Some Bool, Some Bool, Some Bool) -> Some Bool
        | (Some Bool, Some Int, Some Int) -> Some Int
        | _ -> None)