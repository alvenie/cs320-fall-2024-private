open Utils

let rec type_of e =
  match e with
  | Num _ -> Some TInt
  | Add (e1, e2) ->
      (match (type_of e1, type_of e2) with
       | (Some TInt, Some TInt) -> Some TInt
       | _ -> None)
  | Lt (e1, e2) ->
      (match (type_of e1, type_of e2) with
       | (Some TInt, Some TInt) -> Some TBool
       | _ -> None)
  | Ite (cond, true_branch, false_branch) ->
      (match (type_of cond, type_of true_branch, type_of false_branch) with
       | (Some TBool, Some t1, Some t2) when t1 = t2 -> Some t1
       | _ -> None)