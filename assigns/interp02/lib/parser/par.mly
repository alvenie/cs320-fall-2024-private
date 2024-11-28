%{
open Utils

let rec make_fun args body =
  match args with
  | [] -> body
  | (arg, ty) :: rest -> SFun { arg = (arg, ty); args = []; body = make_fun rest body }

let rec make_fun_ty args ret_ty =
  match args with
  | [] -> ret_ty
  | (_, ty) :: rest -> FunTy (ty, make_fun_ty rest ret_ty)
%}

%token <int> NUM
%token <string> VAR
%token LET 
%token REC 
%token IN 
%token IF 
%token THEN 
%token ELSE 
%token FUN 
%token ASSERT
%token UNIT 
%token TRUE 
%token FALSE
%token INT 
%token BOOL 
%token UNIT_TY
%token LPAREN 
%token RPAREN 
%token COLON 
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
%token EOF

%right ARROW
%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  | ts = list(toplet) EOF { ts }

toplet:
  | LET x = VAR args = list(arg) COLON t = ty EQ e = expr
    { { is_rec = false; name = x; args = args; ty = make_fun_ty args t; value = make_fun args e } }
  | LET REC x = VAR arg = arg args = list(arg) COLON t = ty EQ e = expr
    { { is_rec = true; name = x; args = arg :: args; ty = make_fun_ty (arg :: args) t; value = make_fun (arg :: args) e } }

arg:
  | LPAREN x = VAR COLON t = ty RPAREN 
    { (x, t) }

ty:
  | INT 
    { IntTy }
  | BOOL 
    { BoolTy }
  | UNIT_TY 
    { UnitTy }
  | t1 = ty ARROW t2 = ty 
    { FunTy (t1, t2) }
  | LPAREN t = ty RPAREN 
    { t }

expr:
  | LET; x = VAR; args = list(arg); COLON; t = ty; EQ; e1 = expr; IN; e2 = expr
    { SLet { is_rec = false; name = x; args = args; ty = make_fun_ty args t; value = make_fun args e1; body = e2 } }
  | LET; REC; x = VAR; arg = arg; args = list(arg); COLON; t = ty; EQ; e1 = expr; IN; e2 = expr
    { SLet { is_rec = true; name = x; args = arg :: args; ty = make_fun_ty (arg :: args) t; value = make_fun (arg :: args) e1; body = e2 } }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
    { SIf (e1, e2, e3) }
  | FUN; args = nonempty_list(arg); ARROW; e = expr
    { make_fun args e }
  | e = expr2 { e }

expr2:
  | e1 = expr2; op = bop; e2 = expr2 
    { SBop (op, e1, e2) }
  | ASSERT; e = expr3 
    { SAssert e }
  | e = expr3 es = list(expr3) 
    { List.fold_left (fun acc arg -> SApp (acc, arg)) e es }

expr3:
  | UNIT 
    { SUnit }
  | TRUE 
    { STrue }
  | FALSE 
    { SFalse }
  | n = NUM 
    { SNum n }
  | x = VAR 
    { SVar x }
  | LPAREN; e = expr; RPAREN
    { e }

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }