(*
  Code from pa_float camlp4 tutorial http://brion.inria.fr/gallium/index.php/Pa_float
  Creates a syntax extension that converts integers and integer operators to
  floating point operators. Float.(<expr>) will give <expr> with operators
  transformed to floating point operators.

  compile with ocamlc -o pa_float.cmo -I +camlp4 camlp4lib.cma -pp camlp4of.opt  -c pa_float.ml
*)
module Id = struct
  let name = "pa_float"
  let version = "1.0"
end

open Camlp4

(* declare a functor Make *)
module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

(*
  Using Camlp4MapGenerator to traverse the AST.
  See http://brion.inria.fr/gallium/index.php/Camlp4MapGenerator
*)
  class ['a] float_subst _loc = object
    inherit Ast.map as super
    method _Loc_t (_ : 'a) = _loc
    method expr = function
    | <:expr< ( + ) >> -> <:expr< ( +. ) >>
    | <:expr< ( - ) >> -> <:expr< ( -. ) >>
    | <:expr< ( * ) >> -> <:expr< ( *. ) >>
    | <:expr< ( / ) >> -> <:expr< ( /. ) >>
    | <:expr< $int:i$ >> ->
      let f = float(int_of_string i) in <:expr< $`flo:f$ >>
    | e -> super#expr e
  end;;
  EXTEND Gram
    GLOBAL: expr;

    expr: LEVEL "simple"
      [ [ "Float"; "."; "("; e = SELF; ")" -> (new float_subst _loc)#expr e]
      ]
      ;
    END

end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
