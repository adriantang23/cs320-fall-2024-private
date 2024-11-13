open Utils

let parse s = My_parser.parse s

let expr_of_val v = 
    match v with
    | VFun(x, e) -> Fun(x, e)
    | VNum n -> Num n
    | VBool true -> True
    | VBool false -> False
    | VUnit -> Unit

let rec var_replace y x e = 
    match e with
    | Var z -> if z = x then Var y else Var z
    | App (e1,e2) -> App(var_replace y x e1, var_replace y x e2)
    | Fun (z,e) ->
        if z = x then Fun(z, e) else Fun(z, var_replace y x e)
    | Unit -> Unit
    | True -> True
    | False -> False
    | Num n -> Num n
    | Bop (op,e1,e2) -> Bop(op, var_replace y x e1, var_replace y x e2)
    | If (cond,e1,e2) -> If ( var_replace y x cond, var_replace y x e1, var_replace y x e2)
    | Let (z,e1,e2) ->
        if z = x then Let (z, var_replace y x e1,e2) else Let (z, var_replace y x e1, var_replace y x e2)

let rec subst v x e =
    match e with
    | Var z -> 
        if z = x then (expr_of_val v) else Var z
    | App (e1,e2) -> 
        App (subst v x e1, subst v x e2)
    | Fun(y,e) ->
        if x = y then Fun(y,e)
        else
            let y' = gensym() in 
            let e' = var_replace y' y e in
            Fun (y', subst v x e')
    | If (cond, e1, e2) -> If (subst v x cond, subst v x e1, subst v x e2)
    | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)
    | Let (y, e1, e2) -> 
        if x = y then Let (y, subst v x e1, e2) 
        else
            let y' = gensym () in 
            let e2' = var_replace y' y e2 in
            Let (y', subst v x e1, subst v x e2')
    | _ -> e

let bop_eval op v1 v2 = 
    match op, v1, v2 with
    | Add, VNum n1, VNum n2 -> Ok (VNum (n1 + n2))
    | Sub, VNum n1, VNum n2 -> Ok (VNum (n1 - n2))

    | Mul, VNum n1, VNum n2 -> Ok (VNum (n1 * n2))
    | Div, VNum n1, VNum n2 -> 
        if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
    | Mod, VNum n1, VNum n2 -> 
        if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))

    | Lt, VNum n1, VNum n2 -> Ok (VBool (n1 < n2))
    | Lte, VNum n1, VNum n2 -> Ok (VBool (n1 <= n2))
    | Gt, VNum n1, VNum n2 -> Ok (VBool (n1 > n2))
    | Gte, VNum n1, VNum n2 -> Ok (VBool (n1 >= n2))
    | Eq, VNum n1, VNum n2 -> Ok (VBool (n1 = n2))
    | Neq, VNum n1, VNum n2 -> Ok (VBool (n1 <> n2))

    | And, VBool b1, VBool b2 -> Ok (VBool (b1 && b2))
    | Or, VBool b1, VBool b2 -> Ok (VBool (b1 || b2))
    | _, _, _ -> Error (InvalidArgs op)

let rec eval e = 
    match e with
    | Num n -> Ok (VNum n)
    | True -> Ok (VBool true)
    | False -> Ok (VBool false)
    | Unit -> Ok VUnit

    | Var x -> Error (UnknownVar x)
    | Fun (x,e) -> Ok (VFun(x,e))

    | App (e1,e2) -> (
        match eval e1 with
        | Ok (VFun(x,e)) -> (
            match eval e2 with
            | Ok v2 -> eval (subst v2 x e)
            | Error err -> Error err
        ) 
        | Ok _ -> Error InvalidApp
        | Error err -> Error err
    )

    | If (e1,e2,e3) -> (
        match eval e1 with
        | Ok (VBool true) -> eval e2
        | Ok (VBool false) -> eval e3
        | Ok _ -> Error InvalidIfCond
        | Error err -> Error err
    )

    | Bop (Or, e1, e2) -> (
        match eval e1 with
        | Ok (VBool true) -> Ok (VBool true)
        | Ok (VBool false) -> eval e2
        | Ok _ -> Error (InvalidArgs Or)
        | Error err -> Error err
    )
        | Bop (And, e1, e2) -> (
        match eval e1 with
        | Ok (VBool false) -> Ok (VBool false)
        | Ok (VBool true) -> eval e2
        | Ok _ -> Error (InvalidArgs Or)
        | Error err -> Error err
    )
    | Bop (op,e1,e2) -> (
        match eval e1, eval e2 with
        | Ok v1, Ok v2 -> bop_eval op v1 v2
        | Error err, _ -> Error err
        | _, Error err -> Error err
    )

    | Let (x, e1, e2) -> (
        match eval e1 with
        | Ok v1 -> eval (subst v1 x e2)
        | Error err -> Error err
    )

let interp s =
    match parse s with
    | Some e -> eval e
    | None -> Error ParseFail