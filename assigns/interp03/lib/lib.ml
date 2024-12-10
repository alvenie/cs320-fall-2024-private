open Utils
include My_parser

let rec apply_subst_ty (subst: (ident * ty) list) (t: ty): ty =
  match t with
  | TVar x -> 
      (try List.assoc x subst with Not_found -> t)
  | TList t' -> TList (apply_subst_ty subst t')
  | TOption t' -> TOption (apply_subst_ty subst t')
  | TPair (t1, t2) -> TPair (apply_subst_ty subst t1, apply_subst_ty subst t2)
  | TFun (t1, t2) -> TFun (apply_subst_ty subst t1, apply_subst_ty subst t2)
  | _ -> t

let rec free_type_vars (t: ty): ident list =
  let rec unique = function
    | [] -> []
    | h :: t -> h :: unique (List.filter ((<>) h) t)
  in
  match t with
  | TUnit | TInt | TFloat | TBool -> []
  | TVar x -> [x]
  | TList t' | TOption t' -> free_type_vars t'
  | TPair (t1, t2) | TFun (t1, t2) -> 
      let fv1 = free_type_vars t1 in
      let fv2 = free_type_vars t2 in
      unique (List.sort compare (fv1 @ fv2))

let unify (ty: ty) (constrs: constr list): ty_scheme option =
  let rec unify_one (t1: ty) (t2: ty): constr list option =
    match t1, t2 with
    | TUnit, TUnit | TInt, TInt | TFloat, TFloat | TBool, TBool -> Some []
    | TVar x, TVar y when x = y -> Some []
    | TVar x, t when not (occurs x t) -> Some [(TVar x, t)]
    | t, TVar x when not (occurs x t) -> Some [(TVar x, t)]
    | TList t1, TList t2 -> unify_one t1 t2
    | TOption t1, TOption t2 -> unify_one t1 t2
    | TPair (t1a, t1b), TPair (t2a, t2b) ->
        (match unify_one t1a t2a, unify_one t1b t2b with
         | Some c1, Some c2 -> Some (c1 @ c2)
         | _ -> None)
    | TFun (t1a, t1b), TFun (t2a, t2b) ->
        (match unify_one t1a t2a, unify_one t1b t2b with
         | Some c1, Some c2 -> Some (c1 @ c2)
         | _ -> None)
    | _ -> None

  and occurs (x: ident) (t: ty): bool =
    match t with
    | TVar y -> x = y
    | TList t' | TOption t' -> occurs x t'
    | TPair (t1, t2) | TFun (t1, t2) -> occurs x t1 || occurs x t2
    | _ -> false

  and unify_all (constrs: constr list): (ident * ty) list option =
    match constrs with
    | [] -> Some []
    | (t1, t2) :: rest ->
        match unify_one t1 t2 with
        | Some new_constrs ->
            (match unify_all rest with
             | Some subst -> 
                 let combined_subst = 
                   List.map (fun (t1, t2) -> 
                     match t1 with
                     | TVar x -> (x, t2)
                     | _ -> failwith "Invalid constraint in unification"
                   ) new_constrs @ subst
                 in
                 Some (List.map (fun (x, t) -> (x, apply_subst_ty combined_subst t)) combined_subst)
             | None -> None)
        | None -> None

  in
  match unify_all constrs with
  | Some subst -> 
      let final_ty = apply_subst_ty subst ty in
      let free_vars = free_type_vars final_ty in
      Some (Forall (free_vars, final_ty))
  | None -> None

let type_of (env: stc_env) (e: expr): ty_scheme option =
  let fresh_var () = TVar (gensym()) in
  
  let rec go (env: stc_env) (e: expr): (ty * constr list) option =
    match e with
    | Unit -> Some (TUnit, [])
    | True | False -> Some (TBool, [])
    | Int _ -> Some (TInt, [])
    | Float _ -> Some (TFloat, [])
    | Var x -> 
        (match Env.find_opt x env with
         | Some (Forall (vars, t)) -> 
             let subst = List.map (fun v -> (v, fresh_var())) vars in
             Some (apply_subst_ty subst t, [])
         | None -> None)
    | Nil -> 
        let a = fresh_var() in
        Some (TList a, [])
    | ENone -> 
        let a = fresh_var() in
        Some (TOption a, [])
    | ESome e' ->
        (match go env e' with
         | Some (t, c) -> Some (TOption t, c)
         | None -> None)
    | Bop (op, e1, e2) ->
        (match go env e1, go env e2 with
         | Some (t1, c1), Some (t2, c2) ->
             (match op with
              | Add | Sub | Mul | Div | Mod -> 
                  Some (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2)
              | AddF | SubF | MulF | DivF | PowF -> 
                  Some (TFloat, (t1, TFloat) :: (t2, TFloat) :: c1 @ c2)
              | Lt | Lte | Gt | Gte | Eq | Neq -> 
                  Some (TBool, (t1, t2) :: c1 @ c2)
              | And | Or -> 
                  Some (TBool, (t1, TBool) :: (t2, TBool) :: c1 @ c2)
              | Cons -> 
                  let a = fresh_var() in
                  Some (TList a, (t1, a) :: (t2, TList a) :: c1 @ c2)
              | Concat -> 
                  let a = fresh_var() in
                  Some (TList a, (t1, TList a) :: (t2, TList a) :: c1 @ c2)
              | Comma -> 
                  Some (TPair (t1, t2), c1 @ c2))
         | _ -> None)
    | If (e1, e2, e3) ->
        (match go env e1, go env e2, go env e3 with
         | Some (t1, c1), Some (t2, c2), Some (t3, c3) ->
             Some (t2, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3)
         | _ -> None)
    | Fun (x, t_opt, e') ->
        let a = match t_opt with Some t -> t | None -> fresh_var() in
        (match go (Env.add x (Forall ([], a)) env) e' with
         | Some (t, c) -> Some (TFun (a, t), c)
         | None -> None)
    | App (e1, e2) ->
        (match go env e1, go env e2 with
         | Some (t1, c1), Some (t2, c2) ->
             let a = fresh_var() in
             Some (a, (t1, TFun (t2, a)) :: c1 @ c2)
         | _ -> None)
    | Let {is_rec; name; value; body} ->
      if is_rec then
        let a = fresh_var() in
        let b = fresh_var() in
        let env' = Env.add name (Forall ([], TFun (a, b))) env in
        (match go env' value with
         | Some (t1, c1) ->
             let env'' = Env.add name (Forall ([], t1)) env in
             (match go env'' body with
              | Some (t2, c2) -> Some (t2, (t1, TFun (a, b)) :: c1 @ c2)
              | None -> None)
         | None -> None)
      else
        (match go env value with
         | Some (t1, c1) ->
             let env' = Env.add name (Forall ([], t1)) env in
             (match go env' body with
              | Some (t2, c2) -> Some (t2, c1 @ c2)
              | None -> None)
         | None -> None)
    | Assert e' ->
        (match go env e' with
         | Some (t, c) -> Some (TUnit, (t, TBool) :: c)
         | None -> None)
    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
        (match go env matched with
         | Some (t, c) ->
             let a = fresh_var() in
             let env' = Env.add hd_name (Forall ([], a)) (Env.add tl_name (Forall ([], TList a)) env) in
             (match go env' cons_case, go env nil_case with
              | Some (t1, c1), Some (t2, c2) ->
                  Some (t1, (t, TList a) :: (t1, t2) :: c @ c1 @ c2)
              | _ -> None)
         | None -> None)
    | OptMatch { matched; some_name; some_case; none_case } ->
        (match go env matched with
         | Some (t, c) ->
             let a = fresh_var() in
             let env' = Env.add some_name (Forall ([], a)) env in
             (match go env' some_case, go env none_case with
              | Some (t1, c1), Some (t2, c2) ->
                  Some (t1, (t, TOption a) :: (t1, t2) :: c @ c1 @ c2)
              | _ -> None)
         | None -> None)
    | PairMatch { matched; fst_name; snd_name; case } ->
        (match go env matched with
         | Some (t, c) ->
             let a = fresh_var() in
             let b = fresh_var() in
             let env' = Env.add fst_name (Forall ([], a)) (Env.add snd_name (Forall ([], b)) env) in
             (match go env' case with
              | Some (t', c') ->
                  Some (t', (t, TPair (a, b)) :: c @ c')
              | None -> None)
         | None -> None)
    | Annot (e', t) ->
        (match go env e' with
         | Some (t', c) -> Some (t, (t', t) :: c)
         | None -> None)
  in
  match go env e with
  | Some (t, c) -> unify t c
  | None -> None

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

let rec eval_expr (env: dyn_env) (e: expr): value =
  match e with
  | Unit -> VUnit
  | True -> VBool true
  | False -> VBool false
  | Int n -> VInt n
  | Float f -> VFloat f
  | Var x -> Env.find x env
  | Nil -> VList []
  | ENone -> VNone
  | ESome e' -> VSome (eval_expr env e')
  | Bop (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op, v1, v2 with
       | Add, VInt n1, VInt n2 -> VInt (n1 + n2)
       | Sub, VInt n1, VInt n2 -> VInt (n1 - n2)
       | Mul, VInt n1, VInt n2 -> VInt (n1 * n2)
       | Div, VInt n1, VInt n2 -> 
           if n2 = 0 then raise DivByZero else VInt (n1 / n2)
       | Mod, VInt n1, VInt n2 -> 
           if n2 = 0 then raise DivByZero else VInt (n1 mod n2)
       | AddF, VFloat f1, VFloat f2 -> VFloat (f1 +. f2)
       | SubF, VFloat f1, VFloat f2 -> VFloat (f1 -. f2)
       | MulF, VFloat f1, VFloat f2 -> VFloat (f1 *. f2)
       | DivF, VFloat f1, VFloat f2 -> VFloat (f1 /. f2)
       | PowF, VFloat f1, VFloat f2 -> VFloat (f1 ** f2)
       | Lt, _, _ -> VBool (compare_values v1 v2 < 0)
       | Lte, _, _ -> VBool (compare_values v1 v2 <= 0)
       | Gt, _, _ -> VBool (compare_values v1 v2 > 0)
       | Gte, _, _ -> VBool (compare_values v1 v2 >= 0)
       | Eq, _, _ -> VBool (compare_values v1 v2 = 0)
       | Neq, _, _ -> VBool (compare_values v1 v2 <> 0)
       | And, VBool b1, VBool b2 -> VBool (b1 && b2)
       | Or, VBool b1, VBool b2 -> VBool (b1 || b2)
       | Cons, v, VList l -> VList (v :: l)
       | Concat, VList l1, VList l2 -> VList (l1 @ l2)
       | Comma, _, _ -> VPair (v1, v2)
       | _ -> raise (Failure "Invalid operation"))
  | If (e1, e2, e3) ->
      (match eval_expr env e1 with
       | VBool true -> eval_expr env e2
       | VBool false -> eval_expr env e3
       | _ -> raise (Failure "Invalid condition in if expression"))
  | Fun (x, _, e') ->
      VClos { name = None; arg = x; body = e'; env = env }
  | App (e1, e2) ->
      (match eval_expr env e1 with
       | VClos { name; arg; body; env = env' } ->
           let v2 = eval_expr env e2 in
           let env'' = match name with
             | Some f -> Env.add f (VClos { name; arg; body; env = env' }) env'
             | None -> env'
           in
           eval_expr (Env.add arg v2 env'') body
       | _ -> raise (Failure "Invalid function application"))
  | Let { is_rec; name; value; body } ->
      let v = eval_expr env value in
      (match v with
       | VClos { name = Some _; _ } when is_rec -> raise RecWithoutArg
       | _ ->
           let env' = if is_rec
             then Env.add name (VClos { name = Some name; arg = (match v with VClos c -> c.arg | _ -> ""); body = (match v with VClos c -> c.body | _ -> Unit); env = env }) env
             else Env.add name v env
           in
           eval_expr env' body)
  | Assert e' ->
      (match eval_expr env e' with
       | VBool true -> VUnit
       | _ -> raise AssertFail)
  | Annot (e', _) -> eval_expr env e'
  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      (match eval_expr env matched with
       | VList [] -> eval_expr env nil_case
       | VList (hd :: tl) -> 
           let env' = Env.add hd_name hd (Env.add tl_name (VList tl) env) in
           eval_expr env' cons_case
       | _ -> raise (Failure "Invalid list match"))
  | OptMatch { matched; some_name; some_case; none_case } ->
      (match eval_expr env matched with
       | VNone -> eval_expr env none_case
       | VSome v -> 
           let env' = Env.add some_name v env in
           eval_expr env' some_case
       | _ -> raise (Failure "Invalid option match"))
  | PairMatch { matched; fst_name; snd_name; case } ->
      (match eval_expr env matched with
       | VPair (v1, v2) -> 
           let env' = Env.add fst_name v1 (Env.add snd_name v2 env) in
           eval_expr env' case
       | _ -> raise (Failure "Invalid pair match"))

and compare_values v1 v2 =
  match v1, v2 with
  | VClos _, _ | _, VClos _ -> raise CompareFunVals
  | VInt n1, VInt n2 -> compare n1 n2
  | VFloat f1, VFloat f2 -> compare f1 f2
  | VBool b1, VBool b2 -> compare b1 b2
  | VList l1, VList l2 -> compare l1 l2
  | VPair (a1, b1), VPair (a2, b2) ->
      let cmp = compare_values a1 a2 in
      if cmp = 0 then compare_values b1 b2 else cmp
  | VNone, VNone -> 0
  | VNone, VSome _ -> -1
  | VSome _, VNone -> 1
  | VSome v1, VSome v2 -> compare_values v1 v2
  | _ -> raise (Failure "Cannot compare values of different types")

let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError
