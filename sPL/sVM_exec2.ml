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


(* contiguous variable environment in stack form *)
(* TO BE COMPLETED *)
class vENV (size: int) =
object (vv)
  val mutable start_ptr = 0
  val mutable end_ptr = 0
  val mutable high_mark = 0
  val ve = Array.make size BOT
  method add_end_ptr s =
    end_ptr <- end_ptr+s;
    if end_ptr>high_mark then
      high_mark<-end_ptr
    else ()
  method print_high_mark =
    print_endline ("High Stack Memory Mark :"^(string_of_int high_mark))
  (* you may add any new methods below, but should use add_end_ptr
     when you are increasing it *)
  method add v  =
    if end_ptr<size then
      (ve.(end_ptr) <- v;
      vv # add_end_ptr 1)
    else
      failwith "Stack Overflow : vENV"
end;;

(* evm virtual machine implemented as a class *)
class sVML (instSeq:sVML_inst_mc list) size =
object (mc)
  val mutable pc = 0
  val stk = Stack.create ()
  val venv = new vENV size
  val rs = Stack.create ()
    (* rs : (r,pc,venv) stack *)
  val instArr = Array.of_list instSeq
    (* method to check if next inst is DONE *)
  method finish : bool = 
    let c = Array.get instArr pc in
    c == DONE
        (* method to execute one step *)
  method oneStep : unit =
    let c = Array.get instArr pc in
    (* let pr = string_of_int in *)
    Debug.ninfo_hprint (add_str "inst" string_of_sVML_list) [c];
    (* Debug.ninfo_hprint (add_str "pc,venv" (pr_pair pr pr)) (pc,Array.length venv); *)
    failwith "TO BE IMPLEMENTED"
  method execute : value_mc =
    if mc # finish then (venv#print_high_mark;Stack.pop stk)
    else begin mc # oneStep; mc # execute end
end;;

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

(* main program *)
let main =
  (* Read the arguments of command *)
  Arg.parse SPL_type.option_flag (fun s -> file := s) usage; 
  if String.length !file == 0 then print_endline usage else 
  let bytefn = !file^".svm" in
  let _ = print_endline ("Loading sVM code from .."^bytefn) in
  let instr = read_bytecode bytefn in
  let instr_lst = Array.to_list instr in
  let _ = print_endline ("Loaded "^(string_of_sVML_list instr_lst)) in
  let mc = new sVML instr_lst !SPL_type.stack_size in
  let r = mc # execute in
  print_endline ("Executing ==> "^(string_of_sVML_value_mc r))
