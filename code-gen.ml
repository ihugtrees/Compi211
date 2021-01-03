#use "semantic-analyser.ml";;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct
let rec remove_dup lst  = 
  match lst with
  |h::t ->if List.mem h t then (remove_dup t) else h :: (remove_dup t)
  |[] -> []
  
let rec create_free_vars_tuples vars index = 
  match vars with
  | h::t -> [(h,index)]@(create_free_vars_tuples t (index+1))
  | [] -> [];;


let rec find_free_vars ast = 
  match ast with 
        | If' (test , dit , dif) -> (find_free_vars test)@ (find_free_vars dit) @ (find_free_vars dif)
        | Seq' (expr_list) -> List.fold_left (fun (acc) -> (fun (curr) -> acc@curr)) [] (List.map find_free_vars expr_list)
        | Set'((VarFree(str)), expr) -> [str]@(find_free_vars expr)
        | Set' ((VarParam(str , minor)) , e) -> find_free_vars e
        | Set' ((VarBound(str,major , minor)) , e) -> find_free_vars e
        | Or' (expr_list) -> List.fold_left (fun (acc) -> (fun (curr) -> acc@curr)) [] (List.map find_free_vars expr_list)
        | LambdaSimple' (vars , body) -> find_free_vars body
        | LambdaOpt' (vars,opt,body) -> find_free_vars body
        | Applic' (e , expr_list) -> List.fold_left (fun (acc) -> (fun (curr) -> acc@curr)) [] (List.map find_free_vars ([e]@expr_list))
        | ApplicTP'(e ,expr_list) -> List.fold_left (fun (acc) -> (fun (curr) -> acc@curr)) [] (List.map find_free_vars ([e]@expr_list))
        | BoxSet'(VarFree(str) , e) -> [str]@(find_free_vars e)
        | BoxSet'(var , e) -> find_free_vars e
        | Def'((VarFree(str)) , e) -> [str] @ (find_free_vars e)
        | Var'(VarFree(str)) -> [str]
        | _ -> [];;
  let make_consts_tbl asts = raise X_not_yet_implemented;;
    


  let make_fvars_tbl asts = (create_free_vars_tuples (remove_dup (List.flatten (List.map find_free_vars asts)) )0);;
  
  let generate consts fvars e = raise X_not_yet_implemented;;
end;;

