
#use "pc.ml";;
#use "string.ml";;
#use "symbol.ml";;
#use "char.ml";;
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
(* let read_sexprs string = parser (string_to_list string);; *)

let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;



let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function(_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function(e, _) -> e) in
  nt;;

  

let digit_nt = range '0' '9';;
let nt_semi_colon = char ';';;
let nt_end_of_line = char '\n';;
let nt_end_of_file = pack nt_end_of_input (fun _ -> '\n');;


let bool_nt =
  let hash = char '#' in
  let t = char_ci 't' in
  let f = char_ci 'f' in
  let t_f = disj t f in
  let nt = caten hash t_f in
  let nt = pack nt (fun (_,n)-> Bool ((lowercase_ascii n)='t')) in
  nt;;


(*                             Chars                        *)


let char_perfix_nt = word "#\\";;

let named_char_nt =
  disj_list
  [pack (word_ci "nul") (fun _ -> '\000');
   pack (word_ci "newline") (fun _ -> '\010');
   pack (word_ci "return") (fun _ -> '\013');
   pack (word_ci "tab") (fun _ -> '\009');
   pack (word_ci "page") (fun _ -> '\012');
   pack (word_ci "space") (fun _ -> '\032')];;


let visible_simple_char_nt = const (fun c-> c > ' ');;

let char_nt =
  let any_char = (disj named_char_nt visible_simple_char_nt) in 
  pack (caten char_perfix_nt any_char) (fun (_,c)->(c));;

  

(*                             Numberssssssss                        *)

let rec gcd a b =
    if b = 0 then a else gcd b (a mod b);;

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
test_string char_nt "#\\f";;ce) (fun (_,else)->else);; 
*)



(*
test_string number_nt "-8/4";;
test_string number_nt "1.0";;
test_string number_nt "0005.0129";;
test_string number_nt "501.100000000000000000000";;
test_string number_nt "999.12349999999";;
test_string number_nt "-102.000000000000001";;
test_string number_nt "1234";;
test_string char_nt "#\\f";; 
*)


(*                             String                        *)


let nt_string_meta_char =
  disj_list 
  [pack (word_ci "\\\\") (fun _ -> '\092');
  pack (word_ci "\\\"") (fun _ -> '\034');
  pack (word_ci "\\t") (fun _ -> '\009');
  pack (word_ci "\\f") (fun _ -> '\012');
  pack (word_ci "\\n") (fun _ -> '\010');
  pack (word_ci "\\r") (fun _ -> '\013');];;

let nt_string_literal_char =
  diff (diff nt_any (char '\"')) (char '\\');;

let nt_string_char =
  disj nt_string_meta_char nt_string_literal_char ;;

let nt_string =
   pack (caten (caten (char '\"') (star nt_string_char)) (char '\"'))
  (fun ((_,chars),_)-> (list_to_string chars));;


(*                             Symbol                        *)


let nt_symbol_char_no_dot =
  disj_list [char '!';  char '$';  char '^';  char '*';  char '-';  char '_';  char '=';
  char '+'; char '<';  char '>';  char '?';  char '/';  char ':';
  (range 'a' 'z');
  pack (range 'A' 'Z') (fun ch -> lowercase_ascii ch);
  (range '0' '9')];;

let nt_symbol_char = 
  let nt_dot = char '.' in
  disj nt_symbol_char_no_dot nt_dot;;

let nt_symbol =
  let symb = pack (caten nt_symbol_char (plus nt_symbol_char)) (fun(s,lst)->s::lst) in
  let doted = pack symb (fun (chars)->list_to_string chars) in
  let not_doted = pack (plus nt_symbol_char_no_dot) (fun (chars)->list_to_string chars) in
  disj doted not_doted;;

let char_obj = 
  pack char_nt (fun (c)->Char c);;

let symbol_obj = 
  pack nt_symbol (fun(str)->Symbol(str));;

let string_obj = 
  pack nt_string (fun(str)->String(str));;

(*                             List                        *)

let nt_whitespaces = pack (plus nt_whitespace) (fun (a)->' ');;

let nt_line_comment =
  let nt_end = disj nt_end_of_line nt_end_of_file in
  let nt_comment = star (diff nt_any nt_end) in
  let nt_whole_comment = (caten (caten nt_semi_colon nt_comment) nt_end) in 
  pack nt_whole_comment (fun (a)->' ');;

let rec parse_sexpr str =
  (make_paired (star comment_and_space) (star comment_and_space) 
    (disj_list 
          [ 
            bool_nt;
            number_nt;
            char_obj;
            string_obj;
            symbol_obj;
            parse_list;
            parse_dot_list;
            parse_quote;
            parse_qquote;
            parse_unquote_splice;
            parse_unquote
            ]
      )) str

and parse_list str = 
  (pack (caten (char '(') (caten (star parse_sexpr) (char ')')))
  (fun (left,(lst,right))-> match lst with
          | []-> Nil
          | _-> (List.fold_right (fun a b -> Pair (a,b)) lst Nil))) str
          
and parse_dot_list str = 
  let start = caten (char '(') (caten (plus parse_sexpr) (char '.')) in
  let pend = caten parse_sexpr (char ')') in

  (pack (caten start pend) 
  (fun ((l,(s,dot)),(e,r))-> 
    (List.fold_right (fun a b -> Pair (a,b)) s e)
  ))
  str
  
and parse_quote str =
  (pack (caten (char '\'') parse_sexpr)
  (fun (q,s)->Pair(Symbol("quote"),s)))
  str

and parse_qquote str =
  (pack (caten (char '`') parse_sexpr)
  (fun (q,s)->Pair(Symbol("quasiquote"),s)))
  str
  
and parse_unquote_splice str =
  (pack (caten (word ",@") parse_sexpr)
  (fun (q,s)->Pair(Symbol("unquote-splicing"),s)))
  str
  
and parse_unquote str =
  (pack (caten (char ',') parse_sexpr)
  (fun (q,s)->Pair(Symbol("unquote"),s)))
  str
  
and nt_inline_comment str =
  let perfix = word "#;" in
  let comment_sepr = caten perfix parse_sexpr in 
  (pack comment_sepr (fun (_) -> ' ')) str

and comment_and_space str =
     disj_list[nt_whitespaces; nt_line_comment; nt_inline_comment] str;;

let parser s =
  let (res, rest) = (star parse_sexpr) s in
  res;;

parser (string_to_list "(+ 1 2) 1 \"only one\"");;
let read_sexprs string = parser (string_to_list string);;
end;;
 (* struct Reader *) 