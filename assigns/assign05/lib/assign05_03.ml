type ident = string
type ty = 
| Unit
| Arr of ty * ty
type expr = 
| Var of ident
| Fun of ident * ty * expr
| App of expr * expr

let rec type_of gamma e = 
    match e with
    | Var x -> List.assoc_opt x gamma
    | Fun (x, type1, expr1) ->
        (match type_of ((x, type1) :: gamma) expr1 with
        | Some type2 -> Some (Arr(type1,type2))
        | None -> None)
    | App (e1, e2) ->
        (match type_of gamma e1, type_of gamma e2 with
        | Some (Arr (type1, type2)) , Some type1' when type1 = type1' -> Some type2
        | _ -> None)
        