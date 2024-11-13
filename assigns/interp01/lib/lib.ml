open Utils
open My_parser

let rec subst (v: value) (x: string) (e: expr) : expr =
  match e with
  | Var y -> if x = y then value_to_expr v else Var y
  | Num _ | True | False | Unit -> e
  | Fun (y, e1) -> if x = y then Fun (y, e1) else Fun (y, subst v x e1)
  | If (e1, e2, e3) -> If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y, e1, e2) -> 
      if x = y then Let (y, subst v x e1, e2)
      else Let (y, subst v x e1, subst v x e2)
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)

and value_to_expr (v: value) : expr =
  match v with
  | VNum n -> Num n
  | VBool true -> True
  | VBool false -> False
  | VUnit -> Unit
  | VFun (x, e) -> Fun (x, e)


  let rec eval (e: expr) : (value, error) result =
    match e with
    | Num n -> Ok (VNum n)
    | True -> Ok (VBool true)
    | False -> Ok (VBool false)
    | Unit -> Ok VUnit
    | Fun (x, e) -> Ok (VFun (x, e))
    | Var x -> Error (UnknownVar x)
    | If (e1, e2, e3) ->
        (match eval e1 with
         | Ok (VBool true) -> eval e2
         | Ok (VBool false) -> eval e3
         | Ok _ -> Error InvalidIfCond
         | Error e -> Error e)
    | Let (x, e1, e2) ->
        (match eval e1 with
         | Ok v -> eval (subst v x e2)
         | Error e -> Error e)
    | App (e1, e2) ->
        (match eval e1, eval e2 with
         | Ok (VFun (x, e)), Ok v -> eval (subst v x e)
         | Ok _, Ok _ -> Error InvalidApp
         | Error e, _ | _, Error e -> Error e)
    | Bop (op, e1, e2) ->
        match op with
        | And ->
            (match eval e1 with
             | Ok (VBool false) -> Ok (VBool false)
             | Ok (VBool true) -> eval e2
             | Ok _ -> Error (InvalidArgs And)
             | Error e -> Error e)
        | Or ->
            (match eval e1 with
             | Ok (VBool true) -> Ok (VBool true)
             | Ok (VBool false) -> eval e2
             | Ok _ -> Error (InvalidArgs Or)
             | Error e -> Error e)
        | _ ->
            (match eval e1, eval e2 with
             | Ok v1, Ok v2 ->
                 (match op, v1, v2 with
                  | Add, VNum n1, VNum n2 -> Ok (VNum (n1 + n2))
                  | Sub, VNum n1, VNum n2 -> Ok (VNum (n1 - n2))
                  | Mul, VNum n1, VNum n2 -> Ok (VNum (n1 * n2))
                  | Div, VNum n1, VNum n2 when n2 <> 0 -> Ok (VNum (n1 / n2))
                  | Div, _, VNum 0 -> Error DivByZero
                  | Mod, VNum n1, VNum n2 when n2 <> 0 -> Ok (VNum (n1 mod n2))
                  | Mod, _, VNum 0 -> Error DivByZero
                  | Lt, VNum n1, VNum n2 -> Ok (VBool (n1 < n2))
                  | Lte, VNum n1, VNum n2 -> Ok (VBool (n1 <= n2))
                  | Gt, VNum n1, VNum n2 -> Ok (VBool (n1 > n2))
                  | Gte, VNum n1, VNum n2 -> Ok (VBool (n1 >= n2))
                  | Eq, VNum n1, VNum n2 -> Ok (VBool (n1 = n2))
                  | Neq, VNum n1, VNum n2 -> Ok (VBool (n1 <> n2))
                  | _, _, _ -> Error (InvalidArgs op))
             | Error e, _ | _, Error e -> Error e)

let interp (s: string) : (value, error) result =
  match parse s with
  | Some expr -> eval expr
  | None -> Error ParseFail


  %{
    open Utils
    
    let rec mk_app e es = 
      match es with
      | [] -> e
      | x :: es -> mk_app (App (e, x)) es
    %}
    
    %token IF
    %token THEN
    %token ELSE
    %token LET
    %token IN
    %token FUN
    %token ARROW
    %token ADD
    %token SUB
    %token MUL
    %token DIV
    %token MOD 
    %token LT 
    %token LTE 
    %token GT
    %token GTE
    %token EQ
    %token NEQ
    %token AND
    %token OR
    %token LPAREN
    %token RPAREN
    %token FALSE
    %token TRUE
    %token <int> NUM
    %token <string> VAR
    
    %right OR
    %right AND
    %left LT LTE GT GTE EQ NEQ
    %left ADD SUB
    %left MUL DIV MOD
    
    %token EOF
    
    %start <Utils.prog> prog
    
    %%
    
    prog:
      | e = expr; EOF { e }
    
    expr:
      | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
        { If (e1, e2, e3) }
      | LET; x = VAR; EQ; e1 = expr; IN; e2 = expr
        { Let (x, e1, e2) }
      | FUN; x = VAR; ARROW; e = expr
        { Fun (x, e) }
      | e = expr2
        { e }
    
    %inline bop:
      |ADD {Add}
      |SUB {Sub}
      |MUL {Mul}
      |DIV {Div}
      |MOD {Mod}
      |LT {Lt}
      |LTE {Lte}
      |GT {Gt}
      |GTE {Gte}
      |EQ {Eq}
      |NEQ {Neq}
      |AND {And}
      |OR {Or}
      
    expr2:
      | e1 = expr2; op = bop; e2 = expr2
        { Bop (op, e1, e2) }
      | e = expr3; es = expr3*
        { mk_app e es }
    
    expr3:
      | LPAREN; RPAREN
        { Unit }
      | TRUE
        { True }
      | FALSE
        { False }
      | n = NUM
        { Num n }
      | x = VAR
        { Var x }
      | LPAREN; e = expr; RPAREN
        { e }
      
    
    
let parse s =
  try Some (Par.prog Lex.read (Lexing.from_string s))
  with _ -> None