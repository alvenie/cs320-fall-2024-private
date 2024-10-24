open Utils

let rec eval e =
  match e with
  | Num n -> VNum n
  | Add (e1, e2) ->
      (match (eval e1, eval e2) with
       | (VNum v1, VNum v2) -> VNum (v1 + v2)
       | _ -> failwith "Type error: expected numbers for addition")
  | Lt (e1, e2) ->
      (match (eval e1, eval e2) with
       | (VNum v1, VNum v2) -> VBool (v1 < v2)
       | _ -> failwith "Type error: expected numbers for comparison")
  | Ite (cond, true_branch, false_branch) ->
      (match eval cond with
       | VBool c -> if c then eval true_branch else eval false_branch
       | _ -> failwith "Type error: expected boolean for condition")