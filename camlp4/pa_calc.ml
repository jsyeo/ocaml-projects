(*
compile with
-----------
ocamlc -I +camlp4 dynlink.cma camlp4lib.cma -pp camlp4of.opt pa_calc.ml

run with
--------
camlp4 -parser pa_calc.cmo
*)
open Camlp4.PreCast
(*
  gives us the modules Loc, Lexer, Gram, etc
*)

let expression = Gram.Entry.mk "expression"
(* our main rule (entry point)*)

EXTEND Gram
(* extends the Gram grammar *)
  GLOBAL: expression;
(*
  GLOBAL declares "expression" as being a non-local
  grammar entry, while other entries will be local only.
*)
  expression:
    [ "add"
      [ x = SELF; "+"; y = SELF -> x + y
      | x = SELF; "-"; y = SELF -> x - y ]
    | "mul"
      [ x = SELF; "*"; y = SELF -> x * y
      | x = SELF; "/"; y = SELF -> x / y ]
    | "pow" RIGHTA
      [ x = SELF; "**"; y = SELF -> int_of_float ((float x) ** (float y))]
    | "simple"
      [ x = INT -> int_of_string x ] ];
END

let res = Gram.parse_string expression (Loc.mk "<string>") ("1 + 2 * 3");;
let _ = Printf.printf "result ---> %d\n" res
let res = Gram.parse_string expression (Loc.mk "<string>") ("2 * 3 + 1");;
let _ = Printf.printf "result ---> %d\n" res
let res = Gram.parse_string expression (Loc.mk "<string>") ("2 ** 2 ** 2");;
let _ = Printf.printf "result ---> %d\n" res
