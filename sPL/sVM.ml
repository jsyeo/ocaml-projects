open Debug.Basic

(* virtual machine sVM for sPL *)
(* 'label denotes addresses *)
(* 'sym is symbolic name of variables *)
type ('label,'sym) sVML_inst =
  | LABEL of 'label (* similar to NOP *)
  | LDCI of int
  | LDCB of bool
  | PLUS | MINUS | TIMES | DIV | AND | NEG
  | NOT | OR | LT | GT | EQ | DONE
  | NOP
  | GOTO of 'label
  | JOF of 'label
  | LD of 'sym
  | CALL of int
  | TAILCALL of int
  | RTN
  | LDF of 'sym list * int (* * 'sym list *) * 'label
        (* glob_vars * parameters * address *)
  | LDFR of 'sym list * 'sym * int (* * 'sym list *) * 'label
      deriving (Show)
        (* glob_vars * f * parameters * address *)

(* values of sVM *)
type value_mc =
  | BOT
      (* denotes an error *)
  | VInt of int
  | VBool of bool
  | CLS of int * int * var_env
        (* addr * arity * global var values *)
      (* denotes a closure with its environment *)
and var_env = value_mc array
  deriving (Show)

(* type sVML_inst_sym = (string,int) sVML_inst *)
type sVML_inst_mc2 = (int,int) sVML_inst
  deriving (Show)
type sVML_inst_sym = (string,string*int) sVML_inst
  deriving (Show)
type sVML_inst_mc = (int,string*int) sVML_inst
  deriving (Show)

type sVML_prog_mc = sVML_inst_mc list
  deriving (Show)
type sVML_prog_mc2 = sVML_inst_mc2 list
  deriving (Show)
type sVML_prog = sVML_prog_mc
  deriving (Show)
type sVML_prog_sym = sVML_inst_sym list
  deriving (Show)

let iSeq : sVML_prog = [LDCI 1; LDCI 2; PLUS; DONE]

let iArr : sVML_inst_mc array = Array.of_list iSeq

(* let _ = Printf.printf "iseq = %s\n" (Show.show<sVML_prog> iSeq) *)
(* let a = [BOT; CLS (1,2,[|VInt 2|])];; *)
(* let _ = Printf.printf "stk = %s\n" (Show.show<value_mc list> a) *)

(* let string_of_sVML pr_label pr_sym x = *)
(*   match x with *)
(*     | LDCI i -> "LDCI "^(string_of_int i) *)
(*     | LDCB b -> "LDCB "^(string_of_bool b) *)
(*     | LABEL l -> "\n"^(pr_label l)^":" *)
(*     | PLUS -> "PLUS" *)
(*     | MINUS -> "MINUS" *)
(*     | TIMES -> "TIMES" *)
(*     | DIV -> "DIV" *)
(*     | AND -> "AND" *)
(*     | NEG -> "NEG" *)
(*     | NOT -> "NOT" *)
(*     | NOP -> "NOP" *)
(*     | OR -> "OR" *)
(*     | LT -> "LT" *)
(*     | GT  -> "GT" *)
(*     | EQ  -> "EQ" *)
(*     | DONE -> "DONE" *)
(*     | GOTO r -> "GOTO "^(pr_label r) *)
(*     | JOF r -> "JOF "^(pr_label r) *)
(*     | LD i -> "LD "^(pr_sym i) *)
(*     | CALL n -> "CALL "^(string_of_int n) *)
(*     | TAILCALL n -> "TAILCALL "^(string_of_int n) *)
(*     | RTN -> "RTN" *)
(*     | LDF (gvs,a,(\* vs, *\)l) -> *)
(*           let pr = pr_triple (pr_list pr_sym) string_of_int pr_label in *)
(*           ("LDF"^pr(gvs,a,l)) *)
(*     | LDFR (gvs,f,a,(\* vs, *\)l) -> *)
(*           let gvs = (f::gvs) in *)
(*           let pr = pr_triple (pr_list pr_sym) string_of_int pr_label in *)
(*           ("LDFR"^pr(gvs,a,l)) *)

(* (\* printing a list of sVML instructions *\) *)
(* let pr_int_only (_,i) = string_of_int i *)
(* let pr_both = pr_pair pr_id string_of_int *)
(* let pr_x = pr_both *)

(* let string_of_sVML_list (xs:sVML_prog) = *)
(*   pr_list (string_of_sVML string_of_int pr_x ) xs *)

(* let string_of_sVML_list_mc2 (xs:sVML_prog_mc2) = *)
(*   pr_list (string_of_sVML string_of_int string_of_int) xs *)

(* (\* let string_of_sVML_list_sym (xs:sVML_prog_sym) = *\) *)
(* (\*   pr_list (string_of_sVML pr_id string_of_int) xs *\) *)

(* let string_of_sVML_list_sym (xs:sVML_prog_sym) = *)
(*   pr_list (string_of_sVML pr_id pr_x ) xs *)
(* (\* (pr_pair pr_id string_of_int) *\) *)

(* let rec string_of_sVML_value_mc v = *)
(*   match v with *)
(*     | BOT -> "ERR" *)
(*             (\* denotes an error *\) *)
(*     | VInt i -> string_of_int i *)
(*     | VBool b -> string_of_bool b *)
(*     | CLS (addr,arity,e) -> "CLOSURE" *)
(*       (\* "CLOSURE (addr=" ^ string_of_int addr ^ ", arity=" ^ string_of_int arity ^ ", env=" ^ (pr_list string_of_sVML_value_mc (Array.to_list e)) ^ ")" *\) *)
