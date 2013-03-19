open Camlp4
open Camlp4.PreCast

type vec =
| Scalar of string
| Vector of string list
| Sum of vec * vec
| ScalarProduct of vec * vec

let expression = Gram.Entry.mk "expression"

  EXTEND Gram
  GLOBAL: expression;

  expression:
    [ "sum" LEFTA
        [ x = SELF; "+"; y = SELF -> Sum (x,y) ]
    | "scalar" LEFTA
        [ x = SELF; "*"; y = SELF -> ScalarProduct (x,y) ]
    | "simple" NONA
        [ "("; e = SELF; ")" -> e
        | s = scalar -> Scalar s
        | v = vector -> v ] ];

  scalar:
    [ [ `INT (i, _) -> string_of_float (float i)
      | `FLOAT (_, f) -> f ] ];

  vector:
    [ [ "["; v = LIST1 [ s = scalar -> s] SEP ","; "]" -> Vector v ] ];

  END

let _loc = Loc.mk "<string>"

let parse_and_gen_code str =
  let e = Gram.parse_string expression _loc str in e

(* let _ = *)
(*   print_string "# "; *)
(*   let str = read_line() in *)
(*   let e = parse_and_gen_code str in e *)
(*
  We first create a "ast_e" variable which will contain the AST generated by
  the "generate_code" function (the AST of our parsed expression)
  The "Camlp4.PreCast.Printers.OCaml.print_implem" function allows to print
  AST contained in a <:str_item< .. >> quotation.
  In our <:str_item< .. >> quotation we create the "let _" main function. We
  also create a "res" variable whose expression will be the content of our
  "ast_e" variable, i.e. the AST of our parsed expression.
*)
(* let ast_e = <:expr< $e$ >> in *)
(* Camlp4.PreCast.Printers.OCaml.print_implem <:str_item< let _ = let res = $ast_e$ in print_float res >> *)
