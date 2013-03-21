type op_id = string
type id = string


type sPL_type =
  | BoolType
  | IntType
  | Arrow of sPL_type * sPL_type

(* abstract syntax tree for sPL *)
type sPL_expr =
  | BoolConst of bool
  | IntConst of int
  | Var of id
  | UnaryPrimApp of op_id * sPL_expr
  | BinaryPrimApp of op_id * sPL_expr * sPL_expr
  | Cond of sPL_expr * sPL_expr * sPL_expr
  | Func of sPL_type * (id list) * sPL_expr 
        (* min of one parameter *)
  | RecFunc of sPL_type * id * (id list) * sPL_expr 
        (* min of one parameter *)
  | Appln of sPL_expr * sPL_type option * (sPL_expr list) 
        (* at least one argument *)
  | Let of ((sPL_type * id * sPL_expr) list) * sPL_type * sPL_expr
        (* min of one binding; type declaration can be optional *)

open Debug.Basic

(* let pr_id e = e *)
(* let pr_lst s f xs = String.concat s (List.map f xs) *)
(* let pr_list_brk open_b close_b f xs  = open_b ^(pr_lst "," f xs)^close_b *)
(* let pr_list f xs = pr_list_brk "[" "]" f xs *)
(* let pr_opt_bracket p f e =  *)
(*   if p e then "("^(f e)^")" *)
(*   else f e *)

(* display sPL_type *)
(* PLEASE do not change *)
let rec string_of_sPL_type (e:sPL_type):string =
  let pr t =  
    pr_opt_bracket (fun e -> match e with Arrow _ -> true | _ ->false)
        string_of_sPL_type t
  in
  match e with
    | BoolType -> "Bool"
    | IntType -> "Int"
    | Arrow (t1,t2) -> (pr t1)^"->"^(string_of_sPL_type t2)

(* display sPL expr in prefix form *)
(* PLEASE do not change *)
let string_of_sPL (e:sPL_expr):string =
  let pr_type t = "{"^(string_of_sPL_type t)^"}" in
  let rec aux e =
  match e with
    | BoolConst v -> "Bool("^(string_of_bool v)^")"
    | IntConst v -> "Int("^(string_of_int v)^")"
    | Var v -> "Var("^v^")"
    | UnaryPrimApp (op,arg) -> op^"["^(aux arg)^"]"
    | BinaryPrimApp (op,arg1,arg2) -> op^"["^(aux arg1)^","^(aux arg2)^"]"
    | Cond (e1,e2,e3) -> "if "^(aux e1)^" then "^(aux e2)^" else "^(aux e3)
    | Func (t,args,body) -> "fun "^(pr_type t)^" "^(pr_lst " " pr_id args)^" -> "^(aux body)^" end"
    | RecFunc (t,r,args,body) -> "recfun "^r^" "^(pr_type t)^" "^(pr_lst " " pr_id args)^" -> "^(aux body)^" end"
    | Appln (e,t,args) -> "Appln["^(aux e)^"; "^(pr_lst ";" aux args)^"]"
    | Let (lst,t,body) -> 
          let pr (t,v,e) = (pr_type t)^" "^v^" = "^(aux e)
          in "let "^(pr_lst ";" pr lst)^" in "^(pr_type t)^(aux body)^" end"
  in aux e

(* removing vars in ys that occur in xs *)

(* free vars of an expression *)
let rec fv (e:sPL_expr) : id list =
  match e with
    | BoolConst _  | IntConst _ -> []
    | Var i -> [i]
    | UnaryPrimApp (_,arg) -> fv arg
    | BinaryPrimApp (_,arg1,arg2) -> (fv arg1)@(fv arg2)
    | Cond (e1,e2,e3) -> (fv e1)@(fv e2)@(fv e3)
    | Func (_,vs,body) -> diff (fv body) vs
    | RecFunc (_,i,vs,body) -> diff (fv body) (i::vs)
    | Appln (e1,_,es) -> (fv e1)@(List.concat (List.map fv es))
    | Let (lst,_,body) -> 
          let bv = List.map (fun (_,i,_)->i) lst in
          let vs = List.concat ((fv body)::(List.map (fun (_,i,e)->fv e) lst)) in
          diff vs bv

