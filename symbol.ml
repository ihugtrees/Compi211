
#use "pc.ml";;

open PC;;
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

let nt_symbol_char_no_dot =
  disj_list [char '!';  char '$';  char '^';  char '*';  char '-';  char '_';  char '=';
  char '+'; char '<';  char '>';  char '?';  char '/';  char ':';
  (range 'a' 'z');
  (range 'A' 'Z');
  (range '0' '9')];;

let nt_dot = char '.';;

let nt_symbol_char = disj nt_symbol_char_no_dot nt_dot;;

let nt_symbol =
  let symb = pack (caten nt_symbol_char (plus nt_symbol_char)) (fun(s,lst)->s::lst) in
  let doted = pack symb (fun (chars)->list_to_string chars) in
  let not_doted = pack (plus nt_symbol_char_no_dot) (fun (chars)->list_to_string chars) in
  disj doted not_doted;;


test_string nt_symbol "Abc";;
test_string nt_symbol "..";;
test_string nt_symbol ".";;