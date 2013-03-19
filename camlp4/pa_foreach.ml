open Camlp4.PreCast
open Syntax


(*
  turns a list of patterns into variables bounded to functions
  [a;b;c] ==> (\a -> \b -> \c -> expression)
*)
let rec mk_fun _loc patts e =
  match patts with
  | p :: patts ->
    <:expr< fun $p$ -> $mk_fun _loc patts e$ >>
  | [] -> e

let mk_sequence _loc = function
  | <:expr< $_$ ; $_$ >> as e -> <:expr< do { $e$ } >>
  | e -> e
;;

(*
for v in M e do
  seq
done

where module M implements an iterator over the elements in collection e; most of
the time, e also has the type M.t, though this is not strictly required.
*)
EXTEND Gram
  expr: LEVEL "top"
  [ [ "for"; patts = LIST1 ipatt; "in"; m = a_UIDENT; e = expr; "do";
      seq = sequence; "done" ->
      (* use mk_sequence to fix the use do {...} problem *)
      let seq = mk_sequence _loc seq in
      let f = mk_fun _loc patts seq in
      <:expr< $uid:m$.iter $f$ $e$ >>
    ] ]
;
END
