open Assign04_02
type value = 
  | VNum of int
  | VBool of bool

let rec eval e =
  match e with
  | True -> VBool true
  | False -> VBool false
  | Num i -> VNum i
  | Add (n1, n2) ->
      (match eval n1, eval n2 with
      | VNum i1, VNum i2 -> VNum (i1 + i2)
      | _ -> failwith "Type error")
  | Or (e1, e2) ->
      (match eval e1, eval e2 with
      | VBool b1, VBool b2 -> VBool (b1 || b2)
      | _ -> failwith "Type error")
  | IfThenElse (b, e1, e2) ->
      (match eval b with
      | VBool true -> eval e1
      | VBool false -> eval e2
      | _ -> failwith "Type error")

