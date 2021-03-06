OCAMLBUILD = ocamlbuild

#  number of parallel jobs, 0 means unlimited.
JOBS = 0

# dynlink should precede camlp4lib
LIBSB = dynlink,camlp4lib,deriving
LIBSN = dynlink,camlp4lib,deriving
LIBS2 = dynlink,camlp4lib,deriving

INCLUDES = -I,+camlp4

FLAGS = $(INCLUDES),-dtypes
GFLAGS = $(INCLUDES),-g,-dtypes

# -no-hygiene flag to disable "hygiene" rules
OBB_FLAGS = -no-links $(PP) -libs $(LIBSB) -cflags $(FLAGS) -lflags $(FLAGS) -lexflag -q -yaccflag -v  -j $(JOBS) -use-ocamlfind
OBBC_FLAGS = -no-links -libs $(LIBSB) -cflags $(FLAGS) -lflags $(FLAGS) -lexflag -q -yaccflag -v  -j $(JOBS)
OBN_FLAGS = -no-links $(PP) -libs $(LIBSN) -cflags $(FLAGS) -lflags $(FLAGS) -lexflag -q -yaccflag -v  -j $(JOBS)

all: byte

byte: lab08 lab08b

# native: splt.native spli.native

lab06 : splt.byte

lab07 : spli.byte

lab08 : splc.byte svm.byte

lab08b : splc.byte svm2.byte

parser : sPL_parser.cmo

sPL_parser.cmo:
	@ocamlbuild $(OBB_FLAGS) sPL_parser.cmo

splt.byte:
	@ocamlbuild $(OBB_FLAGS) sPL_type_m.byte
	cp _build/sPL_type_m.byte splt

# splt.native:
# 	@ocamlbuild $(OBN_FLAGS) sPL_type_m.native
# 	cp _build/sPL_type_m.native splt

spli.byte:
	@ocamlbuild $(OBB_FLAGS) sPL_inter1.byte sPL_inter2.byte
	cp _build/sPL_inter1.byte spli
	cp _build/sPL_inter2.byte spli2

svm.byte:
	@ocamlbuild $(OBB_FLAGS) sVM_exec.byte
	cp _build/sVM_exec.byte svm

svm2.byte:
	@ocamlbuild $(OBB_FLAGS) sVM_exec2.byte
	cp _build/sVM_exec2.byte svm2

splc.byte:
	@ocamlbuild $(OBB_FLAGS) sPL_compile.byte
	cp _build/sPL_compile.byte splc

# spli.native:
# 	@ocamlbuild $(OBN_FLAGS) sPL_inter1.native sPL_inter2.native
# 	cp -u _build/sPL_inter1.native spli
# 	cp -u _build/sPL_inter2.native spli2

# Clean up
clean:
	$(OCAMLBUILD) -quiet -clean
	rm -f splt spli spli2 splc svm svm2
	rm -f out.s* *cma *.cmo *.cmi *.cmx *.o *.mli *.output *.annot
	# rm sPL_lexer.ml
