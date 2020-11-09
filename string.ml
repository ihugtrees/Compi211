
#use "pc.ml";;
open PC;;
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

let char_in_string = 
  diff nt_any (char '\"')

let string_nt = 
  pack (caten (caten (char '\"') (star char_in_string)) (char '\"')) (fun ((_,chars),_)-> String(list_to_string chars))
