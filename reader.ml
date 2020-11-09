
#use "pc.ml";;
open PC;;
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Fraction of int * int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;

module Reader: sig
  val read_sexprs : string -> sexpr list
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;

let read_sexprs string = raise X_not_yet_implemented;;
end;; (* struct Reader *)


let make_paired nt_left nt_right nt=
  let nt = caten nt_left nt in
  let nt = pack nt (function(_, e) -> e) in 
  let nt = caten nt nt_right in
  let nt = pack nt (function(e, _) -> e) in
  nt;;

let make_spaced nt=
  make_paired (star nt_whitespace) (star nt_whitespace) nt;;


let bool_nt = 
  let hash = char '#' in
  let t = char_ci 't' in
  let f = char_ci 'f' in
  let t_f = disj t f in
  let nt = caten hash t_f in
  let nt = pack nt (fun (_,n)-> Bool ((lowercase_ascii n)='t')) in 
  nt;;

let rec gcd a b =
    if b = 0 then a else gcd b (a mod b);;

let digit_nt = range '0' '9'

let sign_nt = 
  let m = char '-' in
  let p = char '+' in
  disj m p;;

let int_nt =
  let num_nt = pack (plus digit_nt) (fun (str)-> ((int_of_string(list_to_string str)))) in 
  
  let signed = pack (caten sign_nt num_nt) 
  (fun (e,n)-> if (e = '-') then  ((-1)*n) else (n)) in
  pack (disj num_nt signed) (fun (n)->(n)) ;;

let integer_nt =
  let not_frac_and_float = (pack (not_followed_by (not_followed_by  (plus digit_nt) (char '/')) (char '.')) (fun n->int_nt n)) in
  
  pack not_frac_and_float (fun (n,e)->Fraction(n,1));;


let fraction_nt =
  let num = pack (caten int_nt (char '/')) (fun (n,_)->n) in
  let guarded = guard (caten num int_nt) (fun (num,den)->den!=0) in 
  let divided = pack  guarded (fun (num,den)-> let divider = gcd num den in (num/divider,den/divider)) in
  pack divided  (fun (num,den)->Fraction(num,den));;

let float_nt = 
  let float_lst =  caten (plus digit_nt) (caten (char '.') (plus digit_nt)) in
  let num = pack float_lst ((function (a,(b, c)) -> float_of_string ((list_to_string a) ^ "." ^ (list_to_string c)))) in
  let nt_signed = pack (caten sign_nt num) 
    (function (op,num) -> if (op = '-') then (-1.0)*.(num) else num) in
  disj nt_signed num;;

let float_nt_obj =
  pack float_nt (fun (n)->Float n)

  let sci_number_nt = 
    let sci_e = char_ci 'e' in
    let int_num = pack int_nt (fun e -> (float_of_int e)) in
    let num = disj float_nt int_num in
    let remove_e = pack (caten (caten num sci_e) int_nt) (fun ((n,_),m)->n,m) in
    pack remove_e (fun (num,pow)->Float (num *. (10. ** (float_of_int pow))))
  
let number_nt =   
  pack (disj_list [sci_number_nt;fraction_nt;float_nt_obj;integer_nt]) (fun (num)->Number(num));;

(* let nt_spaces = pack (star nt_whitespa
test_string number_nt "-8/4";;
test_string number_nt "1.0";;
test_string number_nt "0005.0129";;
test_string number_nt "501.100000000000000000000";;
test_string number_nt "999.12349999999";;
test_string number_nt "-102.000000000000001";;
test_string number_nt "1234";;
test_string char_nt "#\\f";;ce) (fun (_,else)->else);; *)



let char_perfix_nt = word "#\\";;

let named_char_nt = 
  let nul_nt = word_ci "nul" in
  let newline_nt = word_ci "newline" in
  let return_nt = word_ci "return" in
  let tab_nt = word_ci "tab" in
  let page_nt = word_ci "page" in
  let space_nt = word_ci "space" in
  disj_list [nul_nt; newline_nt; return_nt; tab_nt; page_nt; space_nt];;

let visible_char_nt = const (fun c->c>' ');;
let char_nt =
  
  pack (caten (char_perfix_nt) visible_char_nt) (fun (_,c)->Char(c));;
(* 
test_string number_nt "-8/4";;
test_string number_nt "1.0";;
test_string number_nt "0005.0129";;
test_string number_nt "501.100000000000000000000";;
test_string number_nt "999.12349999999";;
test_string number_nt "-102.000000000000001";;
test_string number_nt "1234";;
test_string char_nt "#\\f";; *)
test_string (make_spaced number_nt) "   2.2E5   ";;
