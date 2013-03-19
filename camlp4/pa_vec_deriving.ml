open Camlp4.PreCast
open Syntax

type vec =
| Scalar of string
| Vector of string list
| Sum of vec * vec
| ScalarProduct of vec * vec
    deriving (Show)

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

let _ = print_string (Show.show <vec> (parse_and_gen_code "[1,2]+[5,6]"));;
