#use "tag-parser.ml";;

type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of var * expr'
  | Def' of var * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq (Var'(var1)) (Var'(var2))) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;

module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
  val get_var_of_vartag : expr' -> var
  val lexical_addressing : expr -> string list -> string list list -> expr'
  val get_index : string -> string list -> int
end;;

module Semantics : SEMANTICS = struct
let get_var_of_vartag v =
  match v with
  | Var'(v) -> v
  | _ -> raise X_syntax_error

let rec lexical_addressing expr params bounds = 
  match expr with
  | Const(const) -> Const'(const)
  | Def(Var(varname), exp) -> Def'(VarFree(varname), (lexical_addressing exp params bounds))
  | If(test, dit, dif) -> If'((lexical_addressing test params bounds), (lexical_addressing dit params bounds), (lexical_addressing dif params bounds))
  | Set(var, exp) -> Set'((get_var_of_vartag (lexical_addressing var params bounds)), (lexical_addressing exp params bounds))
  | Seq(expr_lst) -> Seq'((List.map (fun (expr) -> (lexical_addressing expr params bounds)) expr_lst))
  | Or(expr_lst) -> Or'((List.map (fun (expr) -> (lexical_addressing expr params bounds)) expr_lst))
  | Applic(expr, expr_lst) -> Applic'((lexical_addressing expr params bounds), (List.map (fun (expr)-> (lexical_addressing expr params bounds)) expr_lst))
  | Var(varname) ->if (List.mem varname params)
                    then Var'(VarParam(varname, (get_index varname params)))
                    else if (List.exists (fun (str_lst) -> List.mem varname str_lst) bounds)
                          then   (bound_var varname bounds 0)
                          else   Var'(VarFree(varname))
  | LambdaSimple(vars, exp) -> LambdaSimple'(vars, lexical_addressing exp vars (params::bounds))
  | LambdaOpt(vars, opt_var, exp) -> LambdaOpt'(vars, opt_var, lexical_addressing exp (vars@[opt_var]) (params::bounds))


  and get_index varname lst =
    match lst with
      | [] -> raise X_syntax_error
      | h :: t -> if varname = h then 0 else 1 + get_index varname t


  and bound_var varname bounds idx = 
  match bounds with
  | [] -> raise X_syntax_error
  | h :: t -> if (List.mem varname h) then Var'(VarBound(varname, idx, (get_index varname h))) else (bound_var varname t (idx+1));; 
  

  let rec tail_call expr tp = 
  match expr with
  | Const'(const)-> Const'(const)
  | If' (test, dit, dif) ->  If'( (tail_call test false) , (tail_call dit tp) , (tail_call dif tp))
  | Seq' (expr_lst) ->  Seq' ((List.map (fun (expr)-> (tail_call expr false)) (remove_last expr_lst))@ [(tail_call (get_last expr_lst) tp)]) 
  | Def' (var , expr) -> Def' (var, (tail_call expr tp))
  | Set' (var, expr) -> Set' (var, (tail_call expr false))
  | Or' (expr_lst)-> Or' ((List.map (fun (expr)-> (tail_call expr false)) (remove_last expr_lst))@[(tail_call (get_last expr_lst) tp)]) 
  | LambdaSimple' (vars, body)-> LambdaSimple' (vars, (tail_call body true))
  | LambdaOpt' (vars, opt_var, body)-> LambdaOpt'(vars, opt_var , (tail_call body true))
  | Var' (varname) -> Var' (varname) 
  | Applic'(expr, expr_lst)-> if tp 
                                then ApplicTP'((tail_call expr false), (List.map (fun (expr)-> (tail_call expr false)) expr_lst ))
                                else Applic'((tail_call expr false), (List.map (fun (expr)-> (tail_call expr false))  expr_lst))
  |_-> raise X_syntax_error      
  and remove_last lst =
    match lst with
    | x :: [] -> []
    | x :: tail -> [x]@(remove_last tail)
  and get_last lst =
    match lst with
    | x :: [] -> x
    | x :: tail -> (get_last tail)


let annotate_lexical_addresses e = lexical_addressing e [] [];;

let annotate_tail_calls e = tail_call e true;;

let box_set e = raise X_not_yet_implemented;;

let run_semantics expr =
  (* box_set *)
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)

Semantics.run_semantics 
(
LambdaSimple (["x"; "y"; "z"],
 Applic (Var "+",
  [Var "x"; Var "y";
   LambdaSimple (["z"],
    Applic (Var "+",
     [Var "z";
      LambdaSimple (["x"], Applic (Var "+", [Var "x"; Var "y"; Var "z"]))]))]))
  ) 
= 
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarParam ("z", 0));
      LambdaSimple' (["x"],
       ApplicTP' (Var' (VarFree "+"),
        [Var' (VarParam ("x", 0)); Var' (VarBound ("y", 1, 1));
         Var' (VarBound ("z", 0, 0))]))]))]))

      ;;
