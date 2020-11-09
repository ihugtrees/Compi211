
#use "pc.ml";;

open PC;;
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

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

char_nt ['#';'\\';'t';'a';'b'];;