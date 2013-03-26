let fib n =
  let rec aux a b count =
    if count = n then
      a
    else aux b (a+b) (count + 1)
  in aux 0 1 0
;;

(*
  testing debug module
  ho_1 : (func_name:string) -> (result_printer: 'a -> string) ->
    (arg1_printer : 'a -> string) -> func -> arg1

*)
open Debug;;

let rec fact n =
  if n = 0 then
    1
  else n * fact1 (n-1)
and fact1 n = Debug.ho_1 "fact" string_of_int string_of_int fact n
;;

let _ = fact 12;;
let _ = fib 7;;
