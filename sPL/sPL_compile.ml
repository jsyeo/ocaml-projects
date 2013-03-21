open SPLc
open SVM

(* translating command *)
let trans_cmd op =
  match op with
  | "+" -> PLUS
  | "-" -> MINUS
  | "*" -> TIMES
  | "/" -> DIV
  | "\\" -> NOT
  | "~" -> NEG
  | "|" -> OR
  | "&" -> AND
  | "<" -> LT
  | ">" -> GT
  | "=" -> EQ
  | _ -> failwith ("no such operator "^op)

open Debug.Basic

let labels = new generator "label"

type c_env = int Environ.et

let enum_cenv xs n =
  let rec aux xs n =
    match xs with
    | [] -> []
    | x::xs -> (x,n)::(aux xs (n+1))
  in aux xs n

(* compiling to eVML instrs *)
let compile (e:sPL_expr) : sVML_prog_sym   =
  let rec helper (ce:c_env) (e:sPL_expr) : sVML_prog_sym * sVML_prog_sym  =
    match e with
    | IntConst i -> [LDCI i],[]
    | BoolConst b -> [LDCB b],[]
    | UnaryPrimApp (op,arg) ->
      let (s,p) = helper ce arg in
      (s@[trans_cmd op],p)
    | BinaryPrimApp (op,arg1,arg2) ->
      let (s1,p1) = helper ce arg1 in
      let (s2,p2) = helper ce arg2 in
      (s1@(s2@[trans_cmd op]),p1@p2)
    | Var s ->
      begin
        match Environ.get_val ce s with
        | Some i -> [LD (s,i)],[]
        | None -> [LD (s,(-1))],[]
      end
    | Func (t,vs,body) ->
      let l_fn = labels # fresh_id in
      let fvs = diff (fv body) vs in
      let all_vs = fvs@vs in
      let new_ce = enum_cenv all_vs 0 in
      let arity = List.length vs in
      let (s1,p1) = helper new_ce body in
      let fvs_n = List.map
        (fun v -> match (Environ.get_val ce v) with
        | Some i ->(v,i)
        | _ -> (v,-1)) fvs in
      ([LDF (fvs_n,arity,l_fn)], (((LABEL l_fn)::s1)@[RTN]@p1))
    | Cond (e1,e2,e3) ->
      let else_label = labels # fresh_id in
      let end_if_label = labels # fresh_id in
      let (s1,p1) = helper ce e1 in
      let (s2,p2) = helper ce e2 in
      let (s3,p3) = helper ce e3 in
      (s1@[JOF else_label]@s2@[GOTO end_if_label;LABEL else_label]@s3@[LABEL end_if_label], p1@p2@p3)
    | Appln (f,_,args) ->
      let args_compiled = List.map (helper ce) args in
      let (args_s, args_p) = List.fold_left
        (fun (acc_s, acc_p) (s,p) -> (s::acc_s,p::acc_p)) ([],[]) args_compiled in
      let (args_s, args_p) = (List.flatten args_s, List.flatten args_p) in
      let arity = List.length args in
      let (func_s,func_p) = helper ce f in
      ((args_s @ func_s @ [CALL arity]), args_p@func_p)
    | RecFunc (t,f,vs,body) ->
      let recfunc_label = labels # fresh_id in
      let fvs = diff (fv body) (f::vs) in
      let all_vs = fvs@(f::vs) in
      let new_ce = enum_cenv all_vs 0 in
      let arity = List.length vs in
      let (s1,p1) = helper new_ce body in
      let fvs_n = List.map
        (fun v -> match (Environ.get_val ce v) with
        | Some i -> (v,i)
        | _ -> (v, -1)) fvs in
      let f_id = match (Environ.get_val new_ce f) with
        | Some i -> (f, i)
        | _ -> (f, -1) in
      ([LDFR (fvs_n, f_id, arity, recfunc_label)], ((LABEL recfunc_label)::s1) @ [RTN] @ p1)
  in
  let (main_code,proc_code) = (helper [] e)
  in main_code@(DONE::proc_code)

(* let then_body = SPLc.BinaryPrimApp ("*", SPLc.IntConst 2, SPLc.IntConst 3);; *)
(* let bool_test = SPLc.BinaryPrimApp ("|", SPLc.BoolConst true, SPLc.BoolConst false);; *)
(* compile bool_test;; *)
(* let else_body = SPLc.BinaryPrimApp ("+", SPLc.IntConst 2, SPLc.IntConst 3);; *)
(* let cond = SPLc.Cond (bool_test, then_body, else_body);; *)
(* compile cond;; *)
(* let func_body = SPLc.BinaryPrimApp ("*", SPLc.Var "x", SPLc.Var "y");; *)
(* let func_type = SPL.Arrow (SPL.IntType, SPL.Arrow (SPL.IntType, SPL.IntType));; *)
(* let func_vars = ["x";"y"];; *)
(* let func = SPLc.Func (func_type, func_vars, func_body);; *)
(* let appln = SPLc.Appln (func, func_type, [SPLc.IntConst 4;SPLc.IntConst 3]);; *)
(* compile appln;; *)
let recfunc_type = SPL.Arrow (SPL.IntType, SPL.IntType);;
let recfunc_body = SPLc.Appln (SPLc.Var "f", recfunc_type, [SPLc.BinaryPrimApp ("-", SPLc.Var "x", SPLc.Var "y")]);;
let recfunc_vars = ["x"];;
let recfunc = SPLc.RecFunc (recfunc_type, "f", recfunc_vars, recfunc_body);;
let appln = SPLc.Appln (recfunc, recfunc_type, [SPLc.IntConst 1]);;
compile appln;;
(*
  returns a list of pairs that maps the symbolic
  label name to its position in the instr stack
*)
let collect_label xs =
  let rec aux xs n =
    match xs with
    | [] -> []
    | x::xs ->
      begin
        match x with
        | LABEL s -> (s,n)::(aux xs n)
        | _ -> aux xs (n+1)
      end
  in aux xs 0

(*
  transforms instructions that uses symbolic labels into
  instructions that point to integer indices. Uses the env
  to determine which label at which position. c is the instruction
*)
let trans env c =
  let get_addr l =
    match (Environ.get_val env l) with
    | Some a -> a
    | None -> failwith ("linking problem : unrecognised label :"^l)
  in
  match c with
  | GOTO l -> GOTO (get_addr l)
  | JOF l -> JOF (get_addr l)
  | LABEL l -> LABEL (get_addr l)
  | LDF (g,a,l) -> LDF (g,a,get_addr l)
  | LDFR (g,f,a,l) -> LDFR (g,f,a,get_addr l)
  | LDCI i -> LDCI i
  | LDCB b -> LDCB b
  | PLUS -> PLUS
  | MINUS -> MINUS
  | TIMES -> TIMES
  | DIV -> DIV
  | AND -> AND
  | NEG -> NEG
  | NOT -> NOT
  | NOP -> NOP
  | OR -> OR
  | LT -> LT
  | GT  -> GT
  | EQ  -> EQ
  | DONE -> DONE
  | LD i -> LD i
  | CALL n -> CALL n
  | TAILCALL n -> TAILCALL n
  | RTN -> RTN

let tail_optimize (xs:sVML_prog_sym) : sVML_prog_sym =
  (* to perform tail-call optimization *)
  let rec helper xs =
    match xs with
    | [] -> []
    | (CALL n::RTN::l) -> (TAILCALL n::helper l)
    | (CALL n::LABEL lbl::RTN::l) -> (TAILCALL n::LABEL lbl::RTN::helper l)
    | (a::l) -> (a::helper l)
  in helper xs
;;

tail_optimize (compile appln)
(*
  transforms all instructions to point to integer indices
  instead of symbolic labels
*)
let link_code (xs:sVML_prog_sym) : sVML_prog_mc =
  (* to convert labels to addresses *)
  let env = collect_label xs in
  (List.map (trans env) xs)

(*
  filter out instructions that are labels
*)
let filter_label (xs:sVML_prog_mc) : sVML_prog_mc =
  List.filter (fun c -> match c with LABEL _ -> false | _ -> true) xs


(* output evm bytecode to a file *)
let generate_bytecode(elist: sVML_inst_mc list) (filename:string) =
  let out_channel = open_out_bin filename in
  output_value out_channel (Array.of_list elist);
  close_out out_channel

let generate_bytecode(elist: sVML_inst_mc list) (filename:string) =
  let pr x = string_of_int (List.length x) in
  Debug.no_2 "generate_bytecode" pr pr_id pr_none generate_bytecode elist filename

let extract_filename (s:string) : string =
  let v = String.length s in
  if v<5 then
    failwith "filename at least one char"
  else
    let fn = String.sub s 0 (v-4) in
    let extn = String.sub s (v-4) 4 in
    if extn = ".spl" then fn
    else failwith "filename must have .spl extn"
;;

(* test driver for extract_argument *)
let test_extr_filename () =
  print_endline (extract_filename "hello.epl");
  (* should return "hello" *)
  print_endline (extract_filename ".epl");
  (* should return Failure("filename at least one char") *)
  print_endline (extract_filename "hello.ep")
(* should return Failure("filename must have .epl extn") *)
;;

(* test_extr_filename ();; *)

open SPL_type
module S = SPL

let usage = "usage: " ^ Sys.argv.(0) ^ " [options] <filename>"

(* calling sPL parser *)
let parse_file (filename:string) : (string * S.sPL_expr) =
  SPL_parser.parse_file filename

(* main program *)
let main =
  (* Read the arguments of command *)
  Arg.parse option_flag (fun s -> file := s) usage;
  if String.length !file == 0 then print_endline usage else
    let _ = print_endline "Loading sPL program .." in
    let (s,p) = parse_file !file in
    let _ = print_endline ("  "^s) in
    let _ = print_endline ("  as "^(S.string_of_sPL p)) in
    let _ = print_endline "TYPE CHECKING program .." in
    let (v,np) = type_infer [] p in
    match v with
    | None -> print_endline " ==> type error detected"
    | Some t ->
      print_endline (" ==> inferred type "^(S.string_of_sPL_type t));
      let _ = print_string "TRANSFORMING ==> " in
      let np = trans_exp np in
      let _ = print_endline (string_of_sPL np) in
      let fn = extract_filename !file in
      let bytefn = fn^".svm" in
      let _ = print_string ("COMPILING ==> "^bytefn^"\n") in
      let r = compile np in
      let _ = print_endline (Show.show<sVML_prog_sym> r) in
      let r =
        if !tail_optimize_flag then
          let r = tail_optimize r in
          let _ = print_endline ("TAIL-OPTIMIZE ==> ") in
          let _ = print_endline (Show.show<sVML_prog_sym> r) in
          r
        else r in
      let _ = print_string ("LINKING ==> \n") in
      let s = link_code r in
      let _ = print_endline (Show.show<sVML_prog_mc> s) in
      let s = filter_label s in
      let _ = generate_bytecode s bytefn in
      ()
