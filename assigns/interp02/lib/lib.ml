open Utils
open My_parser

exception DivByZero
exception AssertFail

let rec desugar_sfexpr (e: sfexpr): expr =
  match e with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x
  | SFun { arg = (x, t); args = []; body } ->
      Fun (x, t, desugar_sfexpr body)
  | SFun { arg = (x, t); args; body } ->
      let rec curry_fun args body =
        match args with
        | [] -> desugar_sfexpr body
        | (arg_name, arg_ty) :: rest_args ->
          Fun (arg_name, arg_ty, curry_fun rest_args body)
      in
      Fun (x, t, curry_fun args body)
  | SApp (e1, e2) -> App (desugar_sfexpr e1, desugar_sfexpr e2)
  | SLet { is_rec; name; args; ty; value; body } ->
      let fun_ty = List.fold_right (fun (_, arg_ty) acc -> FunTy (arg_ty, acc)) args ty in
      let fun_value = 
        if args = [] then desugar_sfexpr value
        else
          let fun_body = List.fold_right 
            (fun (arg_name, arg_ty) acc -> SFun { arg = (arg_name, arg_ty); args = []; body = acc })
            args value
          in desugar_sfexpr fun_body
      in
      Let { is_rec; name; ty = fun_ty; value = fun_value; body = desugar_sfexpr body }
  | SIf (e1, e2, e3) -> If (desugar_sfexpr e1, desugar_sfexpr e2, desugar_sfexpr e3)
  | SBop (op, e1, e2) -> Bop (op, desugar_sfexpr e1, desugar_sfexpr e2)
  | SAssert e -> Assert (desugar_sfexpr e)

let desugar (p: prog): expr =
  let rec desugar_toplets = function
    | [] -> Unit
    | toplet :: rest ->
        Let {
          is_rec = toplet.is_rec;
          name = toplet.name;
          ty = List.fold_right (fun (_, arg_ty) acc -> FunTy (arg_ty, acc)) toplet.args toplet.ty;
          value = (match toplet.args with
          | [] -> desugar_sfexpr toplet.value
          | (arg_name, arg_ty) :: rest_args ->
              desugar_sfexpr (SFun { arg = (arg_name, arg_ty); args = rest_args; body = toplet.value }));
          body = desugar_toplets rest
        }
  in
  desugar_toplets p

let type_of (e: expr): (ty, error) result =
  let rec type_of_expr (env: (string * ty) list) (e: expr): (ty, error) result =
    match e with
    | Unit -> Ok UnitTy
    | True | False -> Ok BoolTy
    | Num _ -> Ok IntTy
    | Var x ->
        (match List.assoc_opt x env with
         | Some t -> Ok t
         | None -> Error (UnknownVar x))
    | If (e1, e2, e3) ->
        (match type_of_expr env e1 with
         | Ok BoolTy ->
             (match type_of_expr env e2, type_of_expr env e3 with
              | Ok t2, Ok t3 when t2 = t3 -> Ok t2
              | Ok t2, Ok t3 -> Error (IfTyErr (t2, t3))
              | Error e, _ | _, Error e -> Error e)
         | Ok t -> Error (IfCondTyErr t)
         | Error e -> Error e)
    | Bop (op, e1, e2) ->
        let check_int_op () =
          match type_of_expr env e1, type_of_expr env e2 with
          | Ok IntTy, Ok IntTy -> Ok IntTy
          | Ok IntTy, Ok t -> Error (OpTyErrR (op, IntTy, t))
          | Ok t, _ -> Error (OpTyErrL (op, IntTy, t))
          | Error e, _ -> Error e
        in
        let check_bool_op () =
          match type_of_expr env e1, type_of_expr env e2 with
          | Ok BoolTy, Ok BoolTy -> Ok BoolTy
          | Ok BoolTy, Ok t -> Error (OpTyErrR (op, BoolTy, t))
          | Ok t, _ -> Error (OpTyErrL (op, BoolTy, t))
          | Error e, _ -> Error e
        in
        let check_comp_op () =
          match type_of_expr env e1, type_of_expr env e2 with
          | Ok IntTy, Ok IntTy -> Ok BoolTy
          | Ok IntTy, Ok t -> Error (OpTyErrR (op, IntTy, t))
          | Ok t, _ -> Error (OpTyErrL (op, IntTy, t))
          | Error e, _ -> Error e
        in
        (match op with
         | Add | Sub | Mul | Div | Mod -> check_int_op ()
         | Lt | Lte | Gt | Gte | Eq | Neq -> check_comp_op ()
         | And | Or -> check_bool_op ())
    | Fun (x, t1, e) ->
        (match type_of_expr ((x, t1) :: env) e with
         | Ok t2 -> Ok (FunTy (t1, t2))
         | Error e -> Error e)
    | App (e1, e2) ->
        (match type_of_expr env e1 with
         | Ok (FunTy (t1, t2)) ->
             (match type_of_expr env e2 with
              | Ok t when t = t1 -> Ok t2
              | Ok t -> Error (FunArgTyErr (t1, t))
              | Error e -> Error e)
         | Ok t -> Error (FunAppTyErr t)
         | Error e -> Error e)
    | Let { is_rec; name; ty; value; body } ->
        let env' = (name, ty) :: env in
        let check_value = if is_rec then type_of_expr env' value else type_of_expr env value in
        (match check_value with
         | Ok t when t = ty ->
             type_of_expr env' body
         | Ok t -> Error (LetTyErr (ty, t))
         | Error e -> Error e)
    | Assert e ->
        (match type_of_expr env e with
         | Ok BoolTy -> Ok UnitTy
         | Ok t -> Error (AssertTyErr t)
         | Error e -> Error e)
  in
  type_of_expr [] e

let eval (e: expr) : value =
  let rec eval_in_env (env: value Stdlib320.env) (e: expr) : value =
    match e with
    | Unit -> VUnit
    | True -> VBool true
    | False -> VBool false
    | Num n -> VNum n
    | Var x -> 
      (match Stdlib320.Env.find_opt x env with
       | Some v -> v
       | None -> failwith ("Unbound variable: " ^ x))
    | Bop (op, e1, e2) ->
        let v1 = eval_in_env env e1 in
        let v2 = eval_in_env env e2 in
        (match op, v1, v2 with
         | Add, VNum n1, VNum n2 -> VNum (n1 + n2)
         | Sub, VNum n1, VNum n2 -> VNum (n1 - n2)
         | Mul, VNum n1, VNum n2 -> VNum (n1 * n2)
         | Div, VNum n1, VNum n2 -> 
             if n2 = 0 then raise DivByZero else VNum (n1 / n2)
         | Mod, VNum n1, VNum n2 -> 
             if n2 = 0 then raise DivByZero else VNum (n1 mod n2)
         | Lt, VNum n1, VNum n2 -> VBool (n1 < n2)
         | Lte, VNum n1, VNum n2 -> VBool (n1 <= n2)
         | Gt, VNum n1, VNum n2 -> VBool (n1 > n2)
         | Gte, VNum n1, VNum n2 -> VBool (n1 >= n2)
         | Eq, VNum n1, VNum n2 -> VBool (n1 = n2)
         | Neq, VNum n1, VNum n2 -> VBool (n1 <> n2)
         | And, VBool b1, VBool b2 -> VBool (b1 && b2)
         | Or, VBool b1, VBool b2 -> VBool (b1 || b2)
         | _ -> failwith "Type error in binary operation")
    | If (e1, e2, e3) ->
        (match eval_in_env env e1 with
         | VBool true -> eval_in_env env e2
         | VBool false -> eval_in_env env e3
         | _ -> failwith "Type error in if condition")
    | Fun (x, _, body) -> VClos { name = None; arg = x; body = body; env = env }
  | App (e1, e2) ->
      (match eval_in_env env e1 with
       | VClos { name; arg; body; env = clos_env } ->
           let v2 = eval_in_env env e2 in
           let new_env = Stdlib320.Env.add arg v2 
             (match name with 
              | Some f -> Stdlib320.Env.add f (VClos { name; arg; body; env = clos_env }) clos_env
              | None -> clos_env) in
           eval_in_env new_env body
       | _ -> failwith "Type error in function application")
    | Let { is_rec; name; ty = _; value; body } ->
        let rec_env = if is_rec 
                      then Stdlib320.Env.add name (VClos { name = Some name; arg = ""; body = value; env = env }) env
                      else env in
        let v = eval_in_env rec_env value in
        eval_in_env (Stdlib320.Env.add name v env) body
    | Assert e ->
        (match eval_in_env env e with
         | VBool true -> VUnit
         | VBool false -> raise AssertFail
         | _ -> failwith "Type error in assert")
  in
  eval_in_env Stdlib320.Env.empty e

let interp (s: string) : (value, error) result =
  match parse s with
  | None -> Error ParseErr
  | Some prog ->
      let desugared = desugar prog in
      match type_of desugared with
      | Error err -> Error err
      | Ok _ ->
          try
            Ok (eval desugared)
          with
          | AssertFail -> Error (AssertTyErr BoolTy)
          | DivByZero -> Error (OpTyErrR (Div, IntTy, IntTy))
          
let parse s : prog option = 
  My_parser.parse s