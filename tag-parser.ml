#use "reader.ml";;
open PC;;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;


exception X_syntax_error;;
exception Testing_err;;
module type TAG_PARSER = sig
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
   "unquote-splicing"];;

(* work on the tag parser starts here *)

let rec check_vars vars =
  match vars with
  | Nil -> true
  | Symbol(var) -> not (List.mem var reserved_word_list)
  | Pair(Symbol(var) , rest) -> not (List.mem var reserved_word_list) && check_unique_var var rest  && check_vars rest
  | _ -> raise X_syntax_error

and check_unique_var var vars =
  match vars with
  | Nil -> true
  | Symbol(toCheck) -> not (String.equal var toCheck)
  | Pair(Symbol(toCheck) , rest) -> if String.equal var toCheck then false else check_unique_var var rest
  | _ -> raise X_syntax_error

let rec get_last_var vars =
  match vars with
  | Pair(var, Nil) -> Nil
  | Pair(var, Pair(var1, var2)) -> (get_last_var (Pair(var1, var2)))
  | Pair(var, var1) -> var1
  | _ -> raise X_syntax_error

let rec get_let_vars rib ribs =
  match rib, ribs with
  | Pair(Symbol(symbol), expr), Nil -> Pair(Symbol(symbol), Nil)
  | Pair(Symbol(symbol), expr), Pair(rib, ribs) -> Pair(Symbol(symbol), (get_let_vars rib ribs))
  | _ -> raise X_syntax_error

let rec get_let_sexprs rib ribs =
  match rib, ribs with
  | Pair(Symbol(symbol), Pair(expr, Nil)), Nil -> [expr]
  | Pair(Symbol(symbol), Pair(expr, Nil)), Pair(rib, rest) -> [expr] @ (get_let_sexprs rib rest)
  | _ -> raise X_syntax_error

let rec get_lambda_vars vars =
  match vars with
  | Pair(Symbol(var), Nil) -> [var]
  | Pair(Symbol(var), rest) -> [var] @ (get_lambda_vars rest)
  | _ -> raise X_syntax_error

let rec get_optional_lambda_vars vars =
  match vars with
  | Pair(Symbol(var), Pair(var1, var2)) -> [var] @  (get_optional_lambda_vars (Pair(var1, var2)))
  | Pair(Symbol(var), optional) -> [var]
  | _ -> raise X_syntax_error

let rec letrec_vars rib ribs =
  match rib, ribs with
  | Pair(Symbol(symbol), _), Nil -> Pair(Pair(Symbol(symbol), Pair(Pair(Symbol("quote"), Pair(Symbol("whatever"), Nil)), Nil)), Nil)
  | Pair(Symbol(symbol), _), Pair(rib, ribs) -> Pair(Pair(Symbol(symbol), Pair(Pair(Symbol("quote"), Pair(Symbol("whatever"), Nil)), Nil)), (letrec_vars rib ribs))
  | _-> raise X_syntax_error

let rec letrec_body rib ribs body =
  match rib, ribs with
  | Pair(Symbol(symbol), expr), Nil -> Pair(Pair(Symbol("set!"), Pair(Symbol(symbol), expr)) ,Pair(Pair(Symbol("let"), Pair(Nil, body)),Nil))
  | Pair(Symbol(symbol), expr), Pair(rib, ribs) -> Pair(Pair(Symbol("set!"), Pair(Symbol(symbol), expr)), (letrec_body rib ribs body))
  | _ -> raise X_syntax_error

let rec tag_parse_sexpr sexpr =
  match sexpr with
  | Bool(bool) -> Const(Sexpr(sexpr))
  | Number(num) -> Const(Sexpr(sexpr))
  | Char(char) -> Const(Sexpr(sexpr))
  | String(str) -> Const(Sexpr(sexpr))
  | Nil -> Const(Void)
  | Symbol(symbol) -> if List.mem symbol reserved_word_list then raise X_syntax_error else Var(symbol)
  | Pair(Symbol("if"), Pair(test, Pair(dit, Pair(dif, Nil)))) -> If(tag_parse_sexpr test, tag_parse_sexpr dit, tag_parse_sexpr dif)
  | Pair(Symbol("if"), Pair(test, Pair(dit, Nil))) -> If(tag_parse_sexpr test, tag_parse_sexpr dit, Const(Void))
  | Pair(Symbol("quote"), Pair(var, Nil)) -> Const(Sexpr(var))
  | Pair(Symbol("lambda"), Pair(vars, body)) -> tag_lambda vars body
  | Pair(Symbol("or"), Nil) -> Const(Sexpr(Bool (false)))
  | Pair(Symbol("or"), Pair(expr, Nil)) -> tag_parse_sexpr expr
  | Pair(Symbol("or"), vars) -> Or(tag_or vars)
  | Pair(Symbol("define"), Pair(Symbol(name), Pair(expr, Nil))) -> Def(Var(name), tag_parse_sexpr expr)
  | Pair(Symbol("define"), Pair(Pair(Symbol(name), argl), expr)) -> tag_parse_sexpr (Pair(Symbol("define"), Pair(Symbol(name), Pair(Pair(Symbol("lambda"), Pair(argl, expr)), Nil))))
  | Pair(Symbol("set!"), Pair(Symbol(var), Pair(expr, Nil))) -> Set(Var(var), tag_parse_sexpr expr)
  | Pair(Symbol("pset!"), Pair(rib, ribs)) -> tag_parse_sexpr (tag_pset rib ribs)
  | Pair(Symbol("begin"), Nil) -> Const(Void)
  | Pair(Symbol("begin"), Pair(a, Nil)) -> tag_parse_sexpr a
  | Pair(Symbol("begin"), Pair(exprs, rest)) -> Seq(tag_begin exprs rest)
  | Pair(Symbol("quasiquote"), Pair(sexpr, Nil)) -> tag_parse_sexpr (tag_qquote sexpr)
  | Pair(Symbol("cond"), ribs) -> tag_parse_sexpr (tag_cond ribs)
  | Pair(Symbol("let"), Pair(Nil, body)) -> Applic(tag_parse_sexpr (Pair(Symbol("lambda"), Pair(Nil, body))), [])
  | Pair(Symbol("let"), Pair(Pair(rib, ribs), body)) -> (tag_let rib ribs body)
  | Pair(Symbol("let*"), Pair(bindings, body)) -> tag_parse_sexpr (tag_let_star bindings body)
  | Pair(Symbol("letrec"), Pair(Nil, body)) -> tag_parse_sexpr (Pair (Symbol "let",Pair (Nil,Pair(Pair (Symbol "let",Pair (Nil,body)),Nil))))
  | Pair(Symbol("letrec"), Pair(Pair(rib , ribs) , body)) -> tag_parse_sexpr (tag_letrec rib ribs body)
  | Pair(Symbol("and"), expr) -> tag_and expr
  | Pair(op, vars) -> Applic(tag_parse_sexpr op, tag_applic vars)

  and tag_letrec rib ribs body =
    Pair(Symbol("let"), Pair((letrec_vars rib ribs), (letrec_body rib ribs body)))

  and tag_let rib ribs body =
    Applic((tag_parse_sexpr (Pair(Symbol("lambda"), Pair(get_let_vars rib ribs, body)))),
      (List.map tag_parse_sexpr (get_let_sexprs rib ribs)))

  and tag_applic sexpr =
    match sexpr with
    | Pair(a, Nil) -> [tag_parse_sexpr a]
    | Pair(a, b) -> [tag_parse_sexpr a] @ (tag_applic b)
    | Nil -> []
    | _ -> raise X_syntax_error

  and tag_qquote sexpr =
    match sexpr with
    | Nil -> Pair(Symbol("quote"), Pair(Nil, Nil))
    | Pair(Symbol("unquote"), Pair(x, Nil))-> x
    | Pair(Symbol("unqute-splicing"), Pair(x, Nil))-> raise X_syntax_error
    | Symbol(x) -> Pair(Symbol("quote"), Pair(Symbol(x), Nil))
    | Pair(Pair(Symbol("unquote-splicing"), Pair(a, Nil)), b) -> Pair(Symbol("append"), Pair(a, Pair(tag_qquote b, Nil)))
    | Pair(a, Pair(Symbol("unquote-splicing"), Pair(b, Nil))) -> Pair(Symbol("cons"), Pair(tag_qquote a, Pair(b, Nil)))
    | Pair(a,b) -> (Pair(Symbol("cons"), Pair((tag_qquote a), Pair(tag_qquote b, Nil))))
    | _ -> raise X_syntax_error

  and tag_lambda vars body =
    if check_vars vars then (
    let body_expr = (tag_parse_sexpr (Pair(Symbol("begin"), body))) in
      match vars with
      | Nil -> LambdaSimple([], body_expr)
      | Symbol(optional) -> LambdaOpt([], optional, body_expr)
      | Pair(var, rest) -> (let last_var = get_last_var vars in
        match last_var with
        | Nil -> LambdaSimple(get_lambda_vars vars, body_expr)
        | Symbol(optional) -> LambdaOpt(get_optional_lambda_vars vars, optional, body_expr)
        | _ -> raise X_syntax_error)
      |_ -> raise X_syntax_error
    )
    else raise X_syntax_error

  and tag_let_star binds body =
    match binds with
    | Nil -> Pair(Symbol("let"), Pair(Nil, body))
    | Pair(bind, Nil) ->  Pair(Symbol("let"), Pair(Pair(bind, Nil), body))
    | Pair(bind, binds) ->  Pair(Symbol("let"), Pair(Pair(bind, Nil), Pair(tag_let_star binds body, Nil)))
    | _ -> raise X_syntax_error

  and tag_or sexpr =
    match sexpr with
    | Pair(sexpr, Nil) -> [tag_parse_sexpr sexpr]
    | Pair(sexpr, rest) -> [tag_parse_sexpr sexpr] @ (tag_or rest)
    | _ -> raise X_syntax_error

  and tag_begin sexpr rest =
    match rest with
    | Nil -> [tag_parse_sexpr sexpr]
    | Pair(expr, rest) -> [tag_parse_sexpr sexpr] @ (tag_begin expr rest)
    | _ -> raise X_syntax_error

  and tag_and expr =
    match expr with
    | Nil -> Const(Sexpr(Bool(true)))
    | Pair(expr, Nil) -> tag_parse_sexpr expr
    | Pair(expr, rest) -> If(tag_parse_sexpr expr, tag_parse_sexpr (Pair(Symbol("and"), rest)), Const(Sexpr(Bool(false))))
    | _ -> raise X_syntax_error

  and tag_cond ribs =
    match ribs with
    | Nil -> Nil
    | Pair(Pair(Symbol("else"), dit), rest) -> Pair(Symbol("begin"), dit)
    | Pair(Pair(test, Pair(Symbol("=>"), dit)), Nil) -> Pair(Symbol("let"),
                                                        Pair(Pair(Pair(Symbol("value"), Pair(test, Nil)),
                                                        Pair(Pair(Symbol("f"), Pair(Pair(Symbol("lambda"), Pair(Nil, dit)), Nil)), Nil)),
                                                        Pair(Pair(Symbol("if"), Pair(Symbol("value"), Pair(Pair(Pair(Symbol("f"), Nil), Pair(Symbol("value"), Nil)), Nil))), Nil)))

    | Pair(Pair(test, Pair(Symbol("=>"), dit)), rest) -> Pair(Symbol("let"),
                                                        Pair(Pair(Pair(Symbol("value"), Pair(test, Nil)),
                                                        Pair(Pair(Symbol("f"), Pair(Pair(Symbol("lambda"), Pair(Nil, dit)), Nil)),
                                                        Pair(Pair(Symbol("rest"), Pair(Pair(Symbol("lambda"), Pair(Nil, Pair(tag_cond rest, Nil))), Nil)), Nil))),
                                                        Pair(Pair(Symbol("if"), Pair(Symbol("value"), Pair(Pair(Pair(Symbol("f"), Nil), Pair(Symbol("value"), Nil)), Pair(Pair(Symbol("rest"), Nil), Nil)))), Nil)))

    | Pair(Pair(test, dit), rest) -> Pair(Symbol("if"), Pair(test, Pair(Pair(Symbol("begin"), dit), Pair(tag_cond rest, Nil))))
    | _ -> raise X_syntax_error

  and tag_pset rib ribs =
    Pair(Symbol "let", Pair((pset_lets rib ribs 0),(pset_to_set rib ribs 0)))

  and pset_lets rib ribs count =
    match rib, ribs with
    | Pair(Symbol(symbol), expr), Nil -> Pair(Pair(Symbol("tmp" ^ string_of_int count), expr), Nil)
    | Pair(Symbol(symbol), expr), Pair(rib, rest) ->  Pair(Pair(Symbol("tmp" ^ string_of_int count), expr), (pset_lets rib rest (count+1)))

  and pset_to_set rib ribs count =
    match rib, ribs with
    | Pair(Symbol(symbol), expr), Nil -> Pair(Pair(Symbol("set!"), Pair(Symbol(symbol), Pair(Symbol("tmp" ^ string_of_int count), Nil))),Nil)
    | Pair(Symbol(symbol), expr), Pair(rib, rest) ->  Pair(Pair(Symbol("set!"), Pair(Symbol(symbol), Pair(Symbol("tmp" ^ string_of_int count), Nil))), (pset_to_set rib rest (count+1)))


let tag_parse_expressions sexpr = List.map tag_parse_sexpr sexpr;;

end;; (* struct Tag_Parser *)