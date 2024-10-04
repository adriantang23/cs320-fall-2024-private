
type ident = string
type expr' = 
  | True
  | False
  | Num of int
  | Var of ident
  | Let of ident * expr' * expr'
  | Add of expr' * expr'
  | Or of expr' * expr'
  | IfThenElse of expr' * expr' * expr'
type ty' = 
  | Int
  | Bool
type context = (ident * ty') list

let rec inContext ctxt v =
    match ctxt with
    | [] -> None
    | (a, b) :: tail -> if a = v then Some b else inContext tail v

let rec type_of' gamma e = 
    match e with
    | True | False -> Some Bool
    | Num _ -> Some Int
    | Var v -> inContext gamma v
    | Add (n1, n2) ->
        (match type_of' gamma n1, type_of' gamma n2 with
        | Some Int, Some Int -> Some Int
        | _ -> None)
    | Or (e1, e2) ->
        (match type_of' gamma e1, type_of' gamma e2 with
        | Some Bool, Some Bool -> Some Bool
        | _ -> None)
    | IfThenElse (b, e1, e2) ->
        (match type_of' gamma b, type_of' gamma e1, type_of' gamma e2 with
        | Some Bool, Some t1, Some t2 when t1 = t2 -> Some t1
        | _ -> None)
    | Let (v, e1, e2) ->
        match type_of' gamma e1 with
        | Some ty -> type_of' ((v, ty) :: gamma) e2
        | None -> None

