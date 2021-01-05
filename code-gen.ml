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

let rec find_sexpr_offset sexp const_tbl = 
    match const_tbl with 
    | entry::rest -> let (cur_sexpr, (offset, _)) = entry in
        if (cur_sexpr = sexp) then (string_of_int offset) else find_sexpr_offset sexp rest
    | [] -> "---------WARNING NO SUCH CONST DEFINED-------------";;

let rec expand_sexpr expr =
  match expr with
  | Pair(car,cdr) -> List.append(List.append (expand_sexpr car)(expand_sexpr cdr)) [Sexpr(Pair(car,cdr))]
  | Symbol(s) -> Sexpr(String(s))::[Sexpr(Symbol(s))]
  | sexp -> [Sexpr(sexp)];;

let expand_sexpr_list sexpr_list =
    List.concat (List.map (fun sexpr -> expand_sexpr sexpr) sexpr_list);;

let sexpr_to_const_entry sexpr const_tbl curr_off =
      match sexpr with
      | Void -> ((Void, (curr_off, "MAKE_VOID\n")), curr_off + 1)
      | Sexpr(Nil) -> ((Sexpr(Nil), (curr_off, "MAKE_NIL\n")), curr_off + 1)
      | Sexpr(Char(c)) -> ((Sexpr(Char(c)), (curr_off, "MAKE_LITERAL_CHAR(" ^ (String.make 1 c) ^ ")\n")), curr_off + 2)
      | Sexpr(Bool(false)) -> ((Sexpr(Bool(false)), (curr_off, "MAKE_LITERAL_BOOL(0)\n") ), curr_off + 2)
      | Sexpr(Bool(true)) -> ((Sexpr(Bool(true)), (curr_off, "MAKE_LITERAL_BOOL(1)\n") ), curr_off + 2)
      | Sexpr(Number(Fraction(num, denum))) -> ((Sexpr(Number(Fraction(num, denum))), (curr_off , "MAKE_LITERAL_RATIONAL(" ^ (string_of_int num) ^ "," ^ (string_of_int denum) ^ ")\n")), curr_off + 17)
      | Sexpr(Number(Float(num))) -> ((Sexpr(Number(Float(num))), (curr_off, "MAKE_LITERAL_FLOAT(" ^ (string_of_float num) ^ ")\n")), curr_off + 9)
      | Sexpr(String(str)) -> ((Sexpr(String(str)), (curr_off, "MAKE_LITERAL_STRING " ^ str ^"\n")), curr_off + 9 + (String.length str))
      | Sexpr(Symbol(sym)) -> ((Sexpr(Symbol(sym)), (curr_off, "MAKE_LITERAL_SYMBOL(const_tbl+" ^ (find_sexpr_offset (Sexpr(String(sym))) const_tbl) ^ ")\n")), curr_off + 9)
      | Sexpr(Pair(car, cdr)) -> ((Sexpr(Pair(car,cdr)), (curr_off, "MAKE_LITERAL_PAIR(const_tbl+" ^ (find_sexpr_offset (Sexpr(car)) const_tbl) ^ ",const_tbl+" ^ (find_sexpr_offset (Sexpr(cdr)) const_tbl) ^")\n")), curr_off + 17)

let rec build_const_tbl sexprs const_tbl curr_off =
  match sexprs with
  | sexpr::rest -> let (entry, new_of) = (sexpr_to_const_entry sexpr const_tbl curr_off) in
                      build_const_tbl rest (List.append const_tbl [entry]) new_of
  | [] -> const_tbl

let rec remove_dup lst  =
  match lst with
  |h::t ->if List.mem h t then (remove_dup t) else h :: (remove_dup t)
  |[] -> []

let rec create_free_vars_tuples vars index =
  match vars with
  | h::t -> [(h, index)] @ (create_free_vars_tuples t (index+1))
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

let rec find_const ast =
  match ast with
        | If' (test , dit , dif) -> (find_const test)@ (find_const dit) @ (find_const dif)
        | Seq' (expr_list) -> List.fold_left (fun (acc) -> (fun (curr) -> acc @ curr)) [] (List.map find_const expr_list)
        | Set'(v, expr) -> (find_const expr)
        | Or' (expr_list) -> List.fold_left (fun (acc) -> (fun (curr) -> acc @ curr)) [] (List.map find_const expr_list)
        | LambdaSimple' (vars , body) -> find_const body
        | LambdaOpt' (vars,opt,body) -> find_const body
        | Applic' (e , expr_list) -> List.fold_left (fun (acc) -> (fun (curr) -> acc @ curr)) [] (List.map find_const ([e]@expr_list))
        | ApplicTP'(e ,expr_list) -> List.fold_left (fun (acc) -> (fun (curr) -> acc @ curr)) [] (List.map find_const ([e]@expr_list))
        | BoxSet'(VarFree(str) , e) -> (find_const e)
        | BoxSet'(var , e) -> find_const e
        | Def'((VarFree(str)) , e) -> (find_const e)
        | Var'(VarFree(str)) ->[]
        | Const'(Void) -> []
        | Const'(Sexpr(Symbol(str))) -> [(Sexpr(String(str))) ; (Sexpr(Symbol(str)))]
        | Const'(Sexpr(String(str))) -> [(Sexpr(String(str)))]
        | Const'(Sexpr(Pair(car,cdr))) -> (find_const (Const'(Sexpr(car)))) @ (find_const (Const'(Sexpr(cdr)))) @ [(Sexpr(Pair(car,cdr)))]
        | Const'(Sexpr(Number(n))) ->[Sexpr(Number(n))]
        | _ -> [];;

  let make_consts_tbl asts =
    let consts = (List.rev (remove_dup (List.rev (List.flatten (List.map find_const asts))))) in
    (* let sorted_sexprs_list = expand_sexpr_list consts  in *)
    let sorted_sexprs_list = List.append [Void; Sexpr(Nil) ; Sexpr(Bool(false)) ; Sexpr(Bool(true))] consts in
    let sorted_sexprs_set = remove_dup sorted_sexprs_list in
    build_const_tbl sorted_sexprs_set [] 0 ;;

  let make_fvars_tbl asts = (create_free_vars_tuples (remove_dup (List.flatten (List.map find_free_vars asts))) 0);;

  let generate consts fvars e = raise X_not_yet_implemented;;

 
end;;
(* 
let test_collect_sexp str =
  collect_sexp (Semantics.run_semantics (List.hd (Tag_Parser.tag_parse_expressions (Reader.read_sexprs str))));;

let test_expand_sexp str =
  let sexprs_list = List.fold_left (fun acc ast -> List.append acc (collect_sexp ast)) [] (List.map (fun tag_parsed -> Semantics.run_semantics tag_parsed) (Tag_Parser.tag_parse_expressions (Reader.read_sexprs str))) in
  let sexprs_set = remove_dups sexprs_list [] in
  let sorted_sexprs_list = expand_sexpr_list sexprs_set  in
  let sorted_sexprs_set = remove_dups sorted_sexprs_list [] in
  sorted_sexprs_set ;;

let test_make_const_table str=
  let sexprs_list = List.fold_left (fun acc ast -> List.append acc (collect_sexp ast)) [] (List.map (fun tag_parsed -> Semantics.run_semantics tag_parsed) (Tag_Parser.tag_parse_expressions (Reader.read_sexprs str))) in
  let sexprs_set = remove_dups sexprs_list [] in
  let sorted_sexprs_list = expand_sexpr_list sexprs_set  in
  let sorted_sexprs_list = List.append [ Void; Sexpr(Nil) ; Sexpr(Bool(true)) ; Sexpr(Bool(false))] sorted_sexprs_list in
  let sorted_sexprs_set = remove_dups sorted_sexprs_list [] in
  build_const_tbl sorted_sexprs_set [] 0 ;;

let test_make_fvars_table str =
  let fvars_list = List.fold_left (fun acc ast -> List.append acc (collect_fvars ast)) [] (List.map (fun tag_parsed -> Semantics.run_semantics tag_parsed) (Tag_Parser.tag_parse_expressions (Reader.read_sexprs str))) in
  let fvars_list = List.append fvars_list primitives in
  let fvars_set = remove_dups fvars_list [] in
  build_fvars_tbl fvars_set 0;;
 *)

let ast =  List.map Semantics.run_semantics
                           (Tag_Parser.tag_parse_expressions
                              (Reader.read_sexprs "(list\"ab\" '(1 2) 'c 'ab)"));;
Code_Gen.make_consts_tbl ast;;
(* Code_Gen.make_consts_tbl [
 Const' (Sexpr
   (Pair
     (Pair (Symbol "lambda",
       Pair (Nil,
        Pair
         (Pair (Symbol "lambda",
           Pair (Pair (Symbol "x", Nil),
            Pair (Symbol "x",
             Pair
              (Pair (Symbol "lambda",
                Pair (Nil,
                 Pair
                  (Pair (Symbol "set!",
                    Pair (Symbol "x", Pair (Number (Fraction(1, 1)), Nil))),
                  Nil))),
              Nil)))),
         Nil))),
     Nil)))
] *)
