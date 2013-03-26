all:	vec_deriving
	ocamlc -I +camlp4 dynlink.cma camlp4lib.cma -pp camlp4of.opt pa_calc.ml
	ocamlc -I +camlp4 dynlink.cma camlp4lib.cma -pp camlp4of.opt pa_vec.ml
	ocamlc -I +camlp4 camlp4lib.cma -pp camlp4orf -c pa_foreach.ml
	ocamlc -o pa_float.cmo -I +camlp4 camlp4lib.cma -pp camlp4of.opt  -c pa_float.ml

vec_deriving:
	ocamlc -pp "deriving -parser Camlp4Parsers/Camlp4GrammarParser.cmo" -I +camlp4 -I /Users/jolteon/Dropbox/School/FYP/ocaml_playland/deriving/repos/ja\
ked_deriving/lib dynlink.cma camlp4lib.cma deriving.cma -o vec_deriving pa_vec_deriving.ml

clean:
	rm *.cm[aoi]
	rm vec_deriving
	rm *.out
