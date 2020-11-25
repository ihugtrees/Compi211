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

let rec tag_parse_sexpr sexpr =
  match sexpr with 
  | Bool(bool) -> Const(Sexpr(sexpr))
  | Number(num) -> Const(Sexpr(sexpr))
  | Char(char) -> Const(Sexpr(sexpr))
  | String(str) -> Const(Sexpr(sexpr))
  | Nil -> Const(Void)
  | Symbol(symbol) -> if List.mem symbol reserved_word_list then raise X_syntax_error else Var(sexpr)
  | Pair(Symbol("if"), Pair(test, Pair(dit, Pair(dif, Nil))))->If(tag_parse_sexpr test, tag_parse_sexpr dit, tag_parse_sexpr dif)
  | Pair(Symbol("if"), Pair(test, Pair(dit, Nil))) ->If(tag_parse_sexpr test, tag_parse_sexpr dit, Const(Void))
  | Pair(Symbol("quote"), Pair(x, Nil)) -> Const(Sexpr(x))
  | Pair(Symbol("lambda"), Pair(vars, Pair(body, Nil))) -> LambdaSimple()
  | Pair(Symbol("lambda"), Pair(Nil, Pair(body, Nil))) -> LambdaSimple([],tag_parse_sexpr(Pair(Symbol("begin",body))))
  | Pair(op, vars) -> Applic(tag_parse_sexpr op, )
  | Pair(Symbol("or"), vars) -> Or(tag_or vars)
  | Pair(Symbol("define"), Pair(Symbol(name), Pair(expr, Nil))) -> Def(Var(name), tag_parse_sexpr expr)
  | Pair(Symbol("define"), Pair(Pair(Symbol (name), argl), Pair(expr, Nil))) -> 
  | Pair(Symbol("set!"), Pair(Symbol(var), Pair(expr, Nil))) -> Set(Var(var) tag_parse_sexpr expr)
  | Pair(Symbol("pset!"), Pair(ribs, Nil)) ->
  | Pair(Symbol("begin"), Nil) -> Const(Sexpr(Void))
  | Pair(Symbol("begin"), Pair(Symbol(a), Nil)) -> Var(a)
  | Pair(Symbol("begin"), exprs) -> Seq(tag_begin exprs)
  | Pair(Symbol("quasiquote"), Pair(sexpr, Nil)) ->
  | Pair(Symbol("cond"), ribs) -> 
  | Pair(Symbol("let"), Pair(Nil, Pair(body, Nil))) -> tag_parse_sexpr Pair(Symbol("lambda"), Pair(Nil, Pair(body, Nil)))
  | Pair(Symbol("let"), Pair(Pair(rib, ribs), Pair(body, Nil))) -> 
  | Pair(Symbol("let*"), Pair(bindings, Pair(body, Nil))) -> 
  | Pair(Symbol("letrec"), expr) -> 
  | Pair(Symbol("and"), expr) -> 
  | _ -> raise X_syntax_error

  and tag_or vars =
  match vars with
  | Nil -> Const(Sexpr(Bool(false)))
  | Pair(var, Nil) -> [tag_parse_sexpr var]
  | Pair(var, rest) -> [tag_parse_sexpr var] @ [tag_parse_sexpr rest]
  | _ -> raise X_syntax_error

  and tag_begin exprs =
  match exprs with
  | Pair(expr, Nil) -> [tag_parse_sexpr expr]
  | Pair(expr, rest) -> [tag_parse_sexpr expr] @ tag_begin rest
  | _ -> raise X_syntax_error

  and 


let tag_parse_expressions sexpr = List.map tag_parse_sexpr sexpr;;

  
end;; (* struct Tag_Parser *)

