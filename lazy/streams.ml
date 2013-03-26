let force (lazy v) = v;;

type 'a stream = Nil | Cons of 'a * 'a stream Lazy.t;;

let rec mymap f = function
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, lazy (mymap f (force xs)))
;;

let l = Cons (1, lazy (Cons (2, lazy Nil)));;
let b = mymap (fun x -> x*x) l;;

let inf_ints =
  let rec aux x = Cons (x, lazy (aux (x + 1)))
  in aux 0
;;

let rec countdown n = Cons (n, lazy (countdown (n-1)));;

let rec take n stream =
  match n, stream with
  | _, Nil | 0, _ -> []
  | n, Cons (x, xs) -> x :: (take (n-1) (force xs))
;;

let string_of_int_stream l =
  let rec aux = function
    | Nil -> ""
    | Cons (x, lazy Nil) -> string_of_int x
    | Cons (x, xs) -> (string_of_int x) ^ ";" ^ aux (force xs)
  in "[" ^ aux l ^ "]"
;;

let string_of_float_stream l =
  let rec aux = function
    | Nil -> ""
    | Cons (x, lazy Nil) -> string_of_float x
    | Cons (x, xs) -> (string_of_float x) ^ ";" ^ aux (force xs)
  in "[" ^ aux l ^ "]"
;;

let l = mymap (fun x -> x*x) inf_ints;;
let s = (take 10 l);;
let s = (take 5 (mymap (fun x -> sqrt (float_of_int x)) (countdown 4)));;
