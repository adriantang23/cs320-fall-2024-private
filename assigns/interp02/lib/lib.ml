open Utils

let parse = My_parser.parse

let desugar prog =
  let rec desugar_rec = function
    | SUnit -> Unit
    | STrue -> True
    | SFalse -> False
    | SNum n -> Num n
    | SVar x -> Var x
    | SBop(op, e1, e2) -> Bop(op, desugar_rec e1, desugar_rec e2)
    | SIf(cond, then_, else_) -> If(desugar_rec cond, desugar_rec then_, desugar_rec else_)
    | SApp(e1, e2) -> App(desugar_rec e1, desugar_rec e2)
    | SAssert e -> Assert(desugar_rec e)
    | SFun { arg; args; body } ->
        List.fold_right
          (fun (arg, arg_ty) acc -> Fun(arg, arg_ty, acc))
          (arg :: args)
          (desugar_rec body)
    | SLet { is_rec; name; args; ty; value; body } ->
        let function_type =
          List.fold_right
            (fun (_, arg_ty) acc -> FunTy(arg_ty, acc))
            args
            ty
        in let desugared_value =
          List.fold_right
            (fun (arg, arg_ty) acc -> Fun(arg, arg_ty, acc))
            args
            (desugar_rec value)
        in Let {
          is_rec;
          name;
          ty = function_type;
          value = desugared_value;
          body = desugar_rec body
        }
  and desugar_toplets = function
    | [] -> Unit
    | { is_rec; name; args; ty; value } :: rest ->
        let function_type =
          List.fold_right
            (fun (_, arg_ty) acc -> FunTy(arg_ty, acc))
            args
            ty
        in
        let desugared_value =
          List.fold_right
            (fun (arg, arg_ty) acc -> Fun(arg, arg_ty, acc))
            args
            (desugar_rec value)
        in
        Let { is_rec; name; ty = function_type; value = desugared_value; body = desugar_toplets rest }
  in desugar_toplets prog

let type_of expr =
  let rec ty_rec env expr =
    match expr with
    | Unit -> Ok UnitTy
    | Num _ -> Ok IntTy
    | True | False -> Ok BoolTy
    | Var x -> (
        match Env.find_opt x env with
        | Some ty -> Ok ty
        | None -> Error (UnknownVar x)
    )
    | Let { is_rec; name; ty = expected_ty; value; body } -> (
        if is_rec then (
          let env_ext = Env.add name expected_ty env in
          match ty_rec env_ext value with
          | Ok actual_ty when actual_ty = expected_ty ->
              ty_rec (Env.add name expected_ty env_ext) body
          | Ok actual_ty -> Error (LetTyErr (expected_ty, actual_ty))
          | Error e -> Error e
        ) else (
          match ty_rec env value with
          | Ok actual_ty when actual_ty = expected_ty ->
              ty_rec (Env.add name actual_ty env) body
          | Ok actual_ty -> Error (LetTyErr (expected_ty, actual_ty))
          | Error e -> Error e
        )
    )
    | Fun (arg, arg_ty, body) ->
        let env_ext = Env.add arg arg_ty env in
        (match ty_rec env_ext body with
        | Ok body_ty -> Ok (FunTy (arg_ty, body_ty))
        | Error e -> Error e)
    | App (e1, e2) -> (
        match ty_rec env e1 with
        | Ok (FunTy (arg_ty, ret_ty)) -> (
            match ty_rec env e2 with
            | Ok actual_ty when arg_ty = actual_ty -> Ok ret_ty
            | Ok actual_ty -> Error (FunArgTyErr (arg_ty, actual_ty))
            | Error e -> Error e
        )
        | Ok ty -> Error (FunAppTyErr ty)
        | Error e -> Error e
    )
    | If (cond, then_, else_) -> (
        match ty_rec env cond with
        | Ok BoolTy -> (
            match ty_rec env then_ with
            | Ok ty_then -> (
                match ty_rec env else_ with
                | Ok ty_else when ty_then = ty_else -> Ok ty_then
                | Ok ty_else -> Error (IfTyErr (ty_then, ty_else))
                | Error e -> Error e
            )
            | Error e -> Error e
        )
        | Ok ty -> Error (IfCondTyErr ty)
        | Error e -> Error e
    )
    | Bop (op, e1, e2) -> (
        let (expected_ty1, expected_ty2, result_ty) = match op with
        | Add | Sub | Mul | Div | Mod -> (IntTy, IntTy, IntTy)
        | Lt | Lte | Gt | Gte | Eq | Neq -> (IntTy, IntTy, BoolTy)
        | And | Or -> (BoolTy, BoolTy, BoolTy)
        in
        match ty_rec env e1 with
        | Error e -> Error e 
        | Ok ty1 when ty1 <> expected_ty1 -> Error (OpTyErrL (op, expected_ty1, ty1))
        | Ok _ -> (
            match ty_rec env e2 with
            | Error e -> Error e 
            | Ok ty2 when ty2 <> expected_ty2 -> Error (OpTyErrR (op, expected_ty2, ty2))
            | Ok _ -> Ok result_ty 
        )
    )
    | Assert e -> (
        match ty_rec env e with
        | Ok BoolTy -> Ok UnitTy
        | Ok ty -> Error (AssertTyErr ty)
        | Error e -> Error e
    )
  in
  ty_rec Env.empty expr

exception DivByZero
exception AssertFail

let eval expr =
  let rec eval_expr env expr =
    match expr with
    | Unit -> VUnit
    | Num n -> VNum n
    | True -> VBool true
    | False -> VBool false
    | Var x -> Env.find x env
    | Fun (arg, _, body) -> VClos { name = None; arg; body; env }
    | App (e1, e2) -> (
        match eval_expr env e1 with
        | VClos { name = Some fname; arg; body; env = closure_env } ->
            let v2 = eval_expr env e2 in
            let env_ext =
              Env.add fname (VClos { name = Some fname; arg; body; env = closure_env })
                (Env.add arg v2 closure_env)
            in
            eval_expr env_ext body
        | VClos { name = None; arg; body; env = closure_env } ->
            let v2 = eval_expr env e2 in
            let env_ext = Env.add arg v2 closure_env in
            eval_expr env_ext body
        | _ -> assert false
    )
    | Let { is_rec; name; ty = _; value; body } ->
        let env_ext =
          if is_rec then
            let closure =
              match value with
              | Fun (arg, _, body) -> VClos { name = Some name; arg; body; env }
              | _ ->
                  let gensym_arg = gensym () in
                  let wrapped_body = Fun (gensym_arg, UnitTy, value) in
                  VClos { name = Some name; arg = gensym_arg; body = wrapped_body; env }
            in
            Env.add name closure env
          else
            let v = eval_expr env value in
            Env.add name v env
        in
        eval_expr env_ext body
    | If (cond, then_, else_) -> (
        match eval_expr env cond with
        | VBool true -> eval_expr env then_
        | VBool false -> eval_expr env else_
        | _ -> assert false 
    )
    | Bop (op, e1, e2) -> (
        match op with
        | And -> (
            match eval_expr env e1 with
            | VBool false -> VBool false 
            | VBool true -> eval_expr env e2 
            | _ -> assert false 
        )
        | Or -> (
            match eval_expr env e1 with
            | VBool true -> VBool true 
            | VBool false -> eval_expr env e2 
            | _ -> assert false 
        )
        | _ -> (
            let v1 = eval_expr env e1 in
            let v2 = eval_expr env e2 in
            match (v1, v2, op) with
            | (VNum n1, VNum n2, Add) -> VNum (n1 + n2)
            | (VNum n1, VNum n2, Sub) -> VNum (n1 - n2)
            | (VNum n1, VNum n2, Mul) -> VNum (n1 * n2)
            | (VNum n1, VNum n2, Div) -> if n2 = 0 then raise DivByZero else VNum (n1 / n2)
            | (VNum n1, VNum n2, Mod) -> if n2 = 0 then raise DivByZero else VNum (n1 mod n2)
            | (VNum n1, VNum n2, Lt) -> VBool (n1 < n2)
            | (VNum n1, VNum n2, Lte) -> VBool (n1 <= n2)
            | (VNum n1, VNum n2, Gt) -> VBool (n1 > n2)
            | (VNum n1, VNum n2, Gte) -> VBool (n1 >= n2)
            | (VNum n1, VNum n2, Eq) -> VBool (n1 = n2)
            | (VNum n1, VNum n2, Neq) -> VBool (n1 <> n2)
            | _ -> assert false 
        )
    )
    | Assert e -> (
        match eval_expr env e with
        | VBool true -> VUnit
        | VBool false -> raise AssertFail
        | _ -> assert false 
    )
  in
  eval_expr Env.empty expr

let interp str =
  match parse str with
  | Some prog -> (
      let expr = desugar prog in
      match type_of expr with
      | Ok _ -> Ok (eval expr)
      | Error e -> Error e
  )
  | None -> Error ParseErr
