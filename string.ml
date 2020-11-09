
#use "pc.ml";;
open PC;;
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

let nt_string_meta_char =
  disj_list 
  [pack (word "\\\\") (fun _ -> '\092');
  pack (word "\\\"") (fun _ -> '\034');
  pack (word "\\t") (fun _ -> '\009');
  pack (word "\\f") (fun _ -> '\012');
  pack (word "\\n") (fun _ -> '\010');
  pack (word "\\r") (fun _ -> '\013');];;

let nt_string_literal_char =
  diff (diff nt_any (char '\"')) (char '\\');;

let nt_string_char =
  disj nt_string_meta_char nt_string_literal_char ;;

let nt_string =
   pack (caten (caten (char '\"') (star nt_string_char)) (char '\"'))
  (fun ((_,chars),_)-> (list_to_string chars));;

test_string nt_string "\"sasdasdas\"";;