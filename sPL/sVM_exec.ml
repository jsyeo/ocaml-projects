open SVM
open SPLc
open Debug.Basic

(* read evm bytecode from a file *)
let read_bytecode (filename:string) : sVML_inst_mc array =
  let in_channel = open_in_bin filename in
  let elist = input_value in_channel in
  let _ = close_in in_channel in elist

let get_ints v1 v2 =
  match v1,v2 with
  | VInt v1,VInt v2 -> v1,v2
  | _ -> failwith "type_error : impossible (expecting ints)"

let get_bools v1 v2 =
  match v1,v2 with
  | VBool v1,VBool v2 -> v1,v2
  | _ -> failwith "type_error : impossible (expecting bools)"

let get_bool v1 =
  match v1 with
  | VBool v1 -> v1
  | _ -> failwith "type_error : impossible (expecting bool)"

let get_int v1 =
  match v1 with
  | VInt v1 -> v1
  | _ -> failwith "type_error : impossible (expecting int)"

(* binary operator over integer values *)
let binary_operate (c:sVML_inst_mc) (a1:value_mc) (a2:value_mc) : value_mc =
  match c with
  | PLUS -> let (a1,a2) = get_ints a1 a2 in VInt(a1+a2)
  | MINUS -> let (a1,a2) = get_ints a1 a2 in VInt(a1-a2)
  | TIMES -> let (a1,a2) = get_ints a1 a2 in VInt(a1*a2)
  | DIV ->
    let (a1,a2) = get_ints a1 a2 in
    if a2=0 then BOT
    else VInt(a1/a2)
  | OR ->
    let (a1,a2) = get_bools a1 a2 in VBool (a1||a2)
  | AND ->
    let (a1,a2) = get_bools a1 a2 in VBool (a1&&a2)
  | EQ ->
    let (a1,a2) = get_ints a1 a2 in VBool (a1=a2)
  | LT ->
    let (a1,a2) = get_ints a1 a2 in VBool (a1<a2)
  | GT ->
    let (a1,a2) = get_ints a1 a2 in VBool (a1>a2)
  | _ -> failwith "not possible"

(* unary operator over integer values *)
let unary_operate (c:sVML_inst_mc) (a1:value_mc) : value_mc =
  match c with
  | NEG ->
    let a1 = get_int a1 in VInt (-a1)
  | NOT ->
    let a1 = get_bool a1 in VBool (not(a1))
  | _ -> failwith "not possible"

(* perform operation over a stack *)
(* see mutable OCaml Stack module *)
let proc_inst (stk:value_mc Stack.t) (c:sVML_inst_mc) : unit =
  match c with
  | LDCI i -> Stack.push (VInt i) stk
  | LDCB b -> Stack.push (VBool b) stk
  | PLUS | MINUS | TIMES | AND | OR
  | GT | LT | EQ
    ->
    let a2 = Stack.pop stk in
    let a1 = Stack.pop stk in
    Stack.push (binary_operate c a1 a2) stk
  | NEG | NOT ->
    let a1 = Stack.pop stk in
    Stack.push (unary_operate c a1) stk
  | DIV ->
    begin
      let a2 = Stack.pop stk in
      match a2 with
      | VInt 0 ->
        Stack.push BOT stk;
        failwith "Divide by Zero"
      | _ ->
        let a1 = Stack.pop stk in
        Stack.push (binary_operate c a1 a2) stk
    end
  | _ -> ()


let pop_2_venv stk e m r =
  let rec aux m =
    if m=r then ()
    else
      let v = Stack.pop stk in
      let _ = Array.set e m v in
      aux (m+1)
  in aux m



(* evm virtual machine implemented as a class *)
(*
  type sVML_inst_mc = (int,string*int) sVML_inst
  type sVML_prog_mc = sVML_inst_mc list
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

*)

class sVML (instSeq:sVML_inst_mc list) =
object (mc)
  val mutable pc = 0
  val mutable venv = Array.make 0 BOT
  val op_stk = Stack.create ()
  val runtime_stk = Stack.create ()
  val instArray = Array.of_list instSeq

  method is_finished : bool =
    let instr = Array.get instArray pc in
    instr == DONE

  method one_step : unit =
    begin
      let _ = Printf.printf "pc = %d" pc in
      let instr = Array.get instArray pc in
      match instr with
      | DONE -> ()
      | LABEL _ -> pc <- pc + 1
      | LDCI _ | LDCB _ | PLUS | MINUS | TIMES | DIV
      | AND | NEG | NOT | OR | LT | GT | EQ -> (proc_inst op_stk instr; pc <- pc + 1)
      | NOP -> pc <- pc + 1
      | GOTO n -> pc <- n
      | JOF n ->
        let bool = get_bool (Stack.pop op_stk) in
        if bool then
          pc <- pc + 1
        else
          pc <- n
      | LD (_, index) ->
        let value = Array.get venv index in
        Stack.push value op_stk; pc <- pc + 1
      | CALL n ->
        failwith "not implemented"
      | TAILCALL n ->
        failwith "not implemented"
      | RTN ->
        failwith "not implemented"
      | LDF (globals,arity,loc) ->
        let globals_length = List.length globals in
        let e = Array.init (globals_length)
          (fun i ->
            let index = snd (List.nth globals i) in
            Array.get venv index)
        in (Stack.push (CLS (loc, arity, e)) op_stk; pc <- pc + 1)
      | LDFR (globals, fun_name, arity, loc) ->
        failwith "not implemented"
    end

  method execute : value_mc =
    if mc # is_finished then
      Stack.pop op_stk
    else (mc # one_step ; mc # execute)
end;;

let then_body = SPLc.BinaryPrimApp ("*", SPLc.IntConst 2, SPLc.IntConst 3);;
(* let prog = compile then_body;; *)
(* let bool_test = SPLc.BinaryPrimApp ("|", SPLc.BoolConst false, SPLc.BoolConst false);; *)
(* let else_body = SPLc.BinaryPrimApp ("+", SPLc.IntConst 2, SPLc.IntConst 3);; *)
(* let cond = SPLc.Cond (bool_test, then_body, else_body);; *)
(* let prog = compile cond;; *)
(* let instr = Array.of_list (filter_label (link_code prog)) *)
(* let instr = read_bytecode "mytest.svm" *)
(* let instr_lst = Array.to_list instr *)
(* let mc = new sVML instr_lst;; *)
(* let r = mc # execute;; *)

let usage = "usage: " ^ Sys.argv.(0) ^ " <filename>"
let file = ref ""

(* check that s is of form <fname>.epl *)
(* return <fname> as result *)
(* throw an execption otherwise *)
(* use OCaml methods of String module *)
let extract_filename (s:string) : string =
  let v = String.length s in
  if v<5 then
    failwith "filename at least one char"
  else
    let fn = String.sub s 0 (v-4) in
    let extn = String.sub s (v-4) 4 in
    if extn = ".epl" then fn
    else failwith "filename must have .epl extn"
;;

(* test driver for extract_argument *)
let test_extract_filename () =
  print_endline (extract_filename "hello.epl");
  (* should return "hello" *)
  print_endline (extract_filename ".epl");
  (* should return Failure("filename at least one char") *)
  print_endline (extract_filename "hello.ep")
(* should return Failure("filename must have .epl extn") *)
;;

(* test_extr_filename ();; *)

(* main program *)
let main =
  (* Read the arguments of command *)
  Arg.parse SPL_type.option_flag (fun s -> file := s) usage;
  if String.length !file == 0 then print_endline usage else
    let bytefn = !file^".svm" in
    let _ = print_endline ("Loading sVM code from .."^bytefn) in
    let instr = read_bytecode bytefn in
    let instr_lst = Array.to_list instr in
    let _ = print_endline ("Loaded "^(Show.show<sVML_inst_mc list> instr_lst)) in
    let mc = new sVML instr_lst in
    let r = mc # execute in
    print_endline ("Executing ==> "^(Show.show<value_mc> r))
