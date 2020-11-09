
#use "pc.ml";;

open PC;;
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

let symbol_char_parser = 
  disj_list [char '!';  char '$';  char '^';  char '*';  char '-';  char '_';  char '=';  char '+';
   char '<';  char '>';  char '?';  char '/';  char ':';(range 'a' 'z');(range 'A' 'Z');(range '0' '9')]


let symbol_parser = 
  pack (plus symbol_char_parser) (fun (chars)->Symbol(list_to_string  chars) )