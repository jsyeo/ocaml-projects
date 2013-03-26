(* PLEASE DO NOT CHANGE THIS FILE *)

module Basic =
(* basic utilities that can be opened *)
struct

  let file = ref "" 

  exception Bad_string
  exception Bail

  (* name generator *)
  class generator (s:string) =
  object (slf)
    val mutable cnt = 0
    val str = s
    method fresh_int : int =
      (* generate a new integer *)
      let v = cnt in
      cnt <- v+1; v
    method fresh_id : string =
      (* generate a fresh var id *)
      let i = slf # fresh_int in
      str^"_"^(string_of_int i)
    method fresh_ids (n:int) : string list =
      (* generate a list of n fresh var ids *)
      if n<=0 then []
      else (slf # fresh_id)::(slf # fresh_ids (n-1))
    method fresh_str (s:string) : string =
      let i = slf # fresh_int in
      s^"_"^(string_of_int i)
    method fresh_strs (s:string) (n:int) : string list =
      if n<=0 then []
      else (slf # fresh_str s)::(slf # fresh_strs s (n-1))
  end;;
  let names = new generator "_var"

  type id = string

  module Environ =
  struct 
    type 'b et = (id * 'b) list
    let empty_env : 'b et = []
    let get_val (env:'b et) (v:id) : 'b option =
      try
        Some (snd (List.find (fun (i,_) -> i=v) env))
      with _ -> None
    let size (env:'b et) : int =
      List.length env
          (* let find_posn (env:'b et) (v:id) : int = *)
          (*   (\* pre : i=size(e) *\) *)
          (*   let rec aux e i = *)
          (*     match e with *)
          (*       | [] -> i *)
          (*       | (x,_)::xs -> if v=x then i *)
          (*         else aux xs (i-1)  *)
          (*   in aux env (size env) *)
    let add_env (env:'b et) (v:id) (e:'b) : 'b et =
      (v,e)::env
    let extend_env (env:'b et) (ls:(id*'b) list) : 'b et =
      let rec aux ls e =
        match ls with
          | [] -> e
          | x::xs -> aux xs (x::e)
      in aux ls env
    let build_env (env:'b et) (ls:id list) : 'b et =
      List.filter (fun (i,_) -> List.exists (fun v ->v=i) ls) env
  end;;

  let string_of_pair (p1:'a->string) (p2:'b->string) ((a,b):'a * 'b) : string = 
    "("^(p1 a)^","^(p2 b)^")"

  let rec remove_dups n = 
    match n with
        [] -> []
      | q::qs -> if (List.mem q qs) then remove_dups qs else q::(remove_dups qs)

  let diff (xs:'a list) (ys:'a list) : 'a list =
    let rec aux xs ys =
      match ys with
        | [] -> xs
        | y::ys -> aux (List.filter (fun v -> not(v=y)) xs) ys
    in remove_dups (aux xs ys)

  let pr_id x = x

  let print_flush s = print_endline (s); flush stdout

  let pr_no x = "?"
  let pr_none x = "?"

  let pr_unit x = "()"

  let pr_option f x = match x with
    | None -> "None"
    | Some v -> "Some("^(f v)^")"

  let pr_opt = pr_option 

  let pr_opt_int = pr_option string_of_int

  let pr_pair f1 f2 (x,y) = "("^(f1 x)^","^(f2 y)^")"

  let pr_triple f1 f2 f3 (x,y,z) = "("^(f1 x)^","^(f2 y)^","^(f3 z)^")"

  let pr_quad f1 f2 f3 f4 (x,y,z,z2) = "("^(f1 x)^","^(f2 y)^","^(f3 z)^","^(f4 z2)^")"
  let pr_penta f1 f2 f3 f4 f5 (x,y,z,z2,z3) = "("^(f1 x)^",2:"^(f2 y)^",3:"^(f3 z)^",4:"^(f4 z2)^",5:"^(f5 z3)^")"
  let pr_hexa f1 f2 f3 f4 f5 f6 (x,y,z,z2,z3,z4) = "("^(f1 x)^",2:"^(f2 y)^",3:"^(f3 z)^",4:"^(f4 z2)^",5:"^(f5 z3)^",6:"^(f6 z4)^")"

  let pr_hepta f1 f2 f3 f4 f5 f6 f7 (x,y,z,z2,z3,z4,z5) = "("^(f1 x)^",2:"^(f2 y)^",3:"^(f3 z)^",4:"^(f4 z2)^",5:"^(f5 z3)^",6:"^(f6 z4)^",7:"^(f7 z5)^")"

  let pr_quad_ln f1 f2 f3 f4 (x,y,z,z2) = "("^(f1 x)^"\n,2:"^(f2 y)^"\n,3:"^(f3 z)^"\n,4:"^(f4 z2)^")"
  let pr_penta_ln f1 f2 f3 f4 f5 (x,y,z,z2,z3) = "("^(f1 x)^"\n,2:"^(f2 y)^"\n,3:"^(f3 z)^"\n,4:"^(f4 z2)^"\n,5:"^(f5 z3)^")"
  let pr_hexa_ln f1 f2 f3 f4 f5 f6 (x,y,z,z2,z3,z4) = "("^(f1 x)^"\n,2:"^(f2 y)^"\n,3:"^(f3 z)^"\n,4:"^(f4 z2)^"\n,5:"^(f5 z3)^"\n,6:"^(f6 z4)^")"

  let pr_lst s f xs = String.concat s (List.map f xs)

 let pr_list_brk open_b close_b f xs  = open_b ^(pr_lst "," f xs)^close_b
 let pr_list f xs = pr_list_brk "[" "]" f xs
 let pr_list_angle f xs = pr_list_brk "<" ">" f xs
 let pr_list_round f xs = pr_list_brk "(" ")" f xs
 let pr_list_ln f xs = "["^(pr_lst ",\n" f xs)^"]"

 let pr_opt_bracket p f e =
  if p e then "("^(f e)^")"
  else f e

 let pr_list_mln f xs = (pr_lst "\n--------------\n" f xs)

 let map_opt f x = match x with 
   | None -> None
   | Some v -> Some (f v)

 let map_opt_res f x = match x with 
   | None -> (None,[])
   | Some v -> let r1,r2 = f v in (Some r1,r2)
   
 let fold_opt f x = match x with 
   | None -> []
   | Some v -> (f v)

 let map_l_snd f x = List.map (fun (l,c)-> (l,f c)) x
 let fold_l_snd f x = List.fold_left (fun a (_,c)-> a@(f c)) []  x
 let fold_l_snd_f fj f st x = List.fold_left (fun a (_,c)-> fj a (f c)) st  x
 let map_l_snd_res f x = List.split (List.map (fun (l,c) -> let r1,r2 = f c in ((l,r1),r2)) x)
 let exists_l_snd f x = List.exists (fun (_,c)-> f c) x
 let all_l_snd f x = List.for_all (fun (_,c)-> f c) x
 
 let add_str s f xs = s^":"^(f xs)

  let opt_to_list o = match o with
    | None -> []
    | Some a -> [a]

  let opt_list_to_list o = match o with
    | None -> []
    | Some a -> a

  let fnone (c:'a):'a option = None

  let is_empty l = match l with [] -> true | _ -> false

  let rec last_ne l a  = match l with
    | [] -> a
    | x::xs -> last_ne l x

  let last l = match l with
    | [] -> raise Not_found
    | x::xs -> last_ne l x

  let spacify i = 
    let s' z = List.fold_left (fun x y -> x ^ i ^ y) "" z in
    function [] -> ""
      | [x] -> x
      | x::xs -> x ^ (s' xs)

   (*
    first component of returned value contains the first i values from the list
    second component contains the rest
  *)
  let rec split_at (xs : 'a list) (i : int) : ('a list * 'a list) =
    if i = 0 then ([], xs)
    else
	  let a, b = split_at (List.tl xs) (i-1) in
	  ((List.hd xs) :: a, b) 

  let rec split3 (l : ('a * 'b * 'c) list) : 'a list * 'b list * 'c list = match l with
    | (h1, h2, h3) :: rest ->
	      let l1, l2, l3 = split3 rest in
		  (h1::l1, h2::l2, h3::l3)
    | [] -> ([], [], [])

  let rec combine3 a b c = match (a, b, c) with
    | (ah::arest, bh::brest, ch::crest) ->
	      let l = combine3 arest brest crest in
		  (ah, bh, ch)::l
    | ([], [], []) -> []
    | _ -> failwith ("combine3: combining lists with different lengths")

  let rec map3 (f : 'a -> 'b -> 'c -> 'd) (a0 : 'a list) (bs : 'b list) (cs : 'c list) : 'd list = 
    match (a0, bs, cs) with
	  | (a :: r1, b :: r2, c :: r3) ->
		    let r = map3 f r1 r2 r3 in
		    (f a b c) :: r
	  | [], [], [] -> []
	  | _ -> failwith ("map3: mapping lists with different lengths.")

  let rec map4 (f : 'a -> 'b -> 'c -> 'd -> 'e) (a0 : 'a list) (bs : 'b list) (cs : 'c list) (ds : 'd list) : 'e list = 
    match (a0, bs, cs, ds) with
	  | (a :: r1, b :: r2, c :: r3, d:: r4) ->
		    let r = map4 f r1 r2 r3 r4 in
		    (f a b c d) :: r
	  | [], [], [], [] -> []
	  | _ -> failwith ("map4: mapping lists with different lengths.")


  let rec repeat (v : 'a) (n : int) : 'a list =
    if n <= 0 then []
    else v :: (repeat v (n-1))

end;;

module BList =
struct

  (* List-handling stuff *)

  let string_of_f (f:'a->string) (ls:'a list) : string = 
    ("["^(String.concat "," (List.map f ls))^"]")
    
  (** Split the list of length k>=1 into a pair consisting of
      the list of first k-1 elements and the last element. *)
  let rec firsts_last xs = match xs with
    | [] -> failwith "Gen.first_lasts: empty list"
    | [x] -> ([],x)
    | x::xs1 ->
          let (fs,l) = firsts_last xs1 in
          (x::fs,l)

  let rec take n l  = if n<=0 then [] else 
    match l with
      | h::t -> h::(take (n-1) t)
      | [] -> []
            
  let rec drop n l  = if n<=0 then l else
    match l with
      | h::t -> (drop (n-1) t)
      | [] -> []

  (* let remove_elem e l = List.filter (fun x -> x != e) l *)

  (* let rec remove_dups n =  *)
  (*   match n with *)
  (*       [] -> [] *)
  (*     | q::qs -> if (List.mem q qs) then remove_dups qs else q::(remove_dups qs) *)

  (* let mem f x l = List.exists (f x) l *)

  (* let rec find_dups n =  *)
  (*   match n with *)
  (*     | [] -> [] *)
  (*     | q::qs -> if (List.mem q qs) then q::(find_dups qs) else find_dups qs *)

  (* let subset l1 l2 =  *)
  (*   List.for_all (fun x -> (List.mem x l2)) l1 *)

  (* let disjoint l1 l2 =  *)
  (*   List.for_all (fun x -> not (List.mem x l2)) l1 *)

  (* let overlap eq l1 l2 =  *)
  (*   List.exists (fun x -> (List.mem x l2)) l1 *)

  (* let intersect l1 l2 = *)
  (*   List.filter (fun x -> List.mem x l2) l1 *)


  (* let difference l1 l2 = *)
  (*   List.filter (fun x -> not (List.mem x l2)) l1 *)


  (* let list_equal l1 l2 =  *)
  (*   let l = (List.length (intersect l1 l2)) in *)
  (*   ((List.length l1) =  l) && (l = (List.length l2)) *)


  let find_index (f : 'a -> bool) (xs0 : 'a list) : (int * 'a) = 
    let rec helper xs n = match xs with
	  | e :: rest -> 
		    if f e then (n, e)
		    else helper rest (n + 1)
	  | _ -> raise Not_found
    in
    helper xs0 0

  let rec list_last l = match l with
    | h::[] -> h
    | _::t -> (list_last t)
    | [] -> failwith "Gen.list_last: empty list"

  let remove_elem_eq eq e l = List.filter (fun x -> not(eq x e)) l 

  let mem_eq eq x l = List.exists (eq x) l

  let rec remove_dups_eq eq n = 
    match n with
        [] -> []
      | q::qs -> if (mem_eq eq q qs) then remove_dups_eq eq qs else q::(remove_dups_eq eq qs)

  let rec check_dups_eq eq n = 
    match n with
      | [] -> false
      | q::qs -> if (List.exists (fun c-> eq q c) qs) then true  else check_dups_eq eq qs 

  let check_no_dups_eq eq n = not(check_dups_eq eq n)

  let subset_eq eq l1 l2 =
    List.for_all (fun x -> (mem_eq eq x l2)) l1

  let disjoint_eq eq l1 l2 =
    List.for_all (fun x -> not (mem_eq eq x l2)) l1

  let overlap_eq eq l1 l2 =
	if (l2 == []) then false
	else List.exists (fun x -> (mem_eq eq x l2)) l1

  let rec find_dups_eq eq n = 
    match n with
      | [] -> []
      | q::qs -> if (List.exists (eq q) qs) then q::(find_dups_eq eq qs) else find_dups_eq eq qs

  let rec find_one_dup_eq eq (xs : 'a list) =
    match xs with
	  | [] -> []
	  | x::rest -> if List.exists (eq x) rest then [x] else find_one_dup_eq eq rest

  let mem_eq eq x ls =
    List.exists (fun e -> eq x e) ls

  let intersect_eq eq l1 l2 =
    List.filter (fun x -> List.exists (eq x) l2) l1  

  let difference_eq eq l1 l2 =
    List.filter (fun x -> not (List.exists (eq x) l2)) l1

  let diff_split_eq eq l1 l2 = 
    List.partition (fun x -> not (List.exists (eq x) l2)) l1
    
  let list_subset_eq eq l1 l2 = 
    let l = (List.length (difference_eq eq l1 l2)) in
    l==0

  (* change name to setequal *)
  let list_setequal_eq eq l1 l2 = 
    (list_subset_eq eq l1 l2) && (list_subset_eq eq l2 l1) 

  let list_equiv_eq eq l1 l2 = 
    try
      List.for_all2 eq l1 l2
    with _ -> false

  let rec list_find (f:'a -> 'b option) l = match l with 
    | [] -> None
    | x::xs -> match f x with
        | None -> list_find f xs
        | Some s -> Some s

end;;

include Basic

exception Stack_Error

class ['a] stack  =
   object 
     val mutable stk = []
     method push (i:'a) = 
       begin
         stk <- i::stk
       end
     method get_stk  = stk (* return entire content of stack *)
     method override_stk newstk  = stk <- newstk 
       (* override with a new stack *)
     method pop = match stk with 
       | [] -> print_string "ERROR : popping empty stack"; 
               raise Stack_Error
       | x::xs -> stk <- xs
     method pop_top = match stk with 
       | [] -> print_string "ERROR : popping empty stack"; 
               raise Stack_Error
       | x::xs -> stk <- xs; x
     method top : 'a = match stk with 
       | [] -> print_string "ERROR : top of empty stack"; 
               raise Stack_Error
       | x::xs -> x
     method pop_no_exc = match stk with 
       | [] -> () 
       | x::xs -> stk <- xs
     method is_empty = stk == []
     method len = List.length stk
     method reverse = stk <- List.rev stk
     method mem (i:'a) = List.mem i stk 
     method mem_eq eq (i:'a) = List.exists (fun b -> eq i b) stk 
     (* method exists (i:'a) = List.mem i stk  *)
     (* method exists_eq eq (i:'a) = List.exists (fun b -> eq i b) stk  *)
     method exists f = List.exists f stk 
     method push_list (ls:'a list) =  stk <- ls@stk
     method pop_list (ls:'a list) = 
       stk <- BList.drop (List.length ls) stk
     method reset = stk <- []
   end;;

class ['a] stack_pr (epr:'a->string) (eq:'a->'a->bool)  =
   object 
     inherit ['a] stack as super
     val elem_pr = epr 
     val elem_eq = eq 
     method string_of = Basic.pr_list_ln elem_pr stk
     method string_of_no_ln = Basic.pr_list elem_pr stk
     method string_of_reverse = let _ = super#reverse  in
                                Basic.pr_list_ln elem_pr stk
     method string_of_reverse_log = let _ = super#reverse  in
                                Basic.pr_list_mln elem_pr stk
     method mem (i:'a) = List.exists (elem_eq i) stk
     method overlap (ls:'a list) = 
	   if (ls == []) then false
	   else List.exists (fun x -> List.exists (elem_eq x) ls) stk
   end;;


class ['a] stack_filter (epr:'a->string) (eq:'a->'a->bool) (fil:'a->bool)  =
   object 
     inherit ['a] stack_pr epr eq as super
     val filter_fn = fil
     method filter = stk <- List.filter fil stk
     method string_of_reverse_log_filter = 
       stk <- List.filter fil stk;
       super#string_of_reverse_log
   end;;

class ['a] stack_noexc (x_init:'a) (epr:'a->string) (eq:'a->'a->bool)  =
   object 
     inherit ['a] stack_pr epr eq
     val emp_val = x_init
     method top_no_exc : 'a = match stk with 
       | [] ->  emp_val
       | x::xs -> x
   end;;

class counter x_init =
   object 
     val mutable ctr = x_init
     method get : int = ctr
     method inc = ctr <- ctr + 1
     method inc_and_get = ctr <- ctr + 1; ctr
     method add (i:int) = ctr <- ctr + i
     method reset = ctr <- 0
     method string_of : string= (string_of_int ctr)
   end;;

module StackTrace =
struct 
  (* keep track of calls being traced by ho_debug *)
  let ctr = new counter 0
    
  (* type stack = int list *)
  (* stack of calls being traced by ho_debug *)
  let debug_stk = new stack_noexc (-2) string_of_int (=)

  let dd_stk = new stack

  (* let force_dd_print () = *)
  (*   let d = dd_stk # get_stk in *)
  (*   debug_stk # overlap d *)

  let is_same_dd_get () =
    if dd_stk # is_empty then None
    else 
      let v1 = dd_stk # top in
      let v2 = debug_stk # top in
       if (v1==v2) then Some v1 else None

  let is_same_dd () =
    match (is_same_dd_get()) 
    with | None -> false
      | _ -> true

  (* pop last element from call stack of ho debug *)
  let pop_call () = 
    if is_same_dd () then dd_stk # pop;
    debug_stk # pop

  (* call f and pop its trace in call stack of ho debug *)
  let pop_aft_apply_with_exc (f:'a->'b) (e:'a) : 'b =
    let r = (try 
      (f e)
    with exc -> (pop_call(); raise exc))
    in pop_call(); r

  (* call f and pop its trace in call stack of ho debug *)
  let pop_aft_apply_with_exc_no (f:'a->'b) (e:'a) : 'b =
    let r = (try 
      (f e)
    with exc -> (debug_stk # pop; raise exc))
    in debug_stk # pop; r

  (* string representation of call stack of ho_debug *)
  let string_of () : string =
    let h = debug_stk#get_stk in
    (* ("Length is:"^(string_of_int (List.length h))) *)
    String.concat "@" (List.map string_of_int (List.filter (fun n -> n>0) h) )

  let push_no_call () =
    debug_stk # push (-1)

  (* returns @n and @n1;n2;.. for a new call being debugged *)
  let push_call_gen (os:string) (flag:bool) : (string * string) = 
    ctr#inc;
    let v = ctr#get in
    debug_stk#push v; if flag then dd_stk#push v;
    let s = os^"@"^(string_of_int v) in
    let h = os^"@"^string_of() in
    (* let _ = print_endline ("push_call:"^os^":"^s^":"^h) in  *)
    s,h

  let push_call (os:string) : (string * string) = 
    push_call_gen os false

  let push_call_dd (os:string) : (string * string) = 
    push_call_gen os true

end;;

(* open Globals *)
type loc =  {
    start_pos : Lexing.position (* might be expanded to contain more information *);
    mid_pos : Lexing.position;
    end_pos : Lexing.position;
  }

let no_pos = 
	let no_pos1 = { Lexing.pos_fname = "";
				   Lexing.pos_lnum = 0;
				   Lexing.pos_bol = 0; 
				   Lexing.pos_cnum = 0 } in
	{start_pos = no_pos1; mid_pos = no_pos1; end_pos = no_pos1;}

let debug_on = ref false
let devel_debug_on = ref false
let devel_debug_print_orig_conseq = ref false

let log_devel_debug = ref false
let debug_log = Buffer.create 5096

let clear_debug_log () = Buffer.clear debug_log
let get_debug_log () = Buffer.contents debug_log

(* debugging facility for user *)

(* used to enable the printing of the original consequent while devel debugging. By default, orig_conseq is disabled*)
let enable_dd_and_orig_conseq_printing () =
 devel_debug_on := true;
 devel_debug_print_orig_conseq :=  true

let string_of_pos (pos:loc) =
  pos.start_pos.Lexing.pos_fname ^ ":" ^ (string_of_int pos.start_pos.Lexing.pos_lnum) ^ ": "^(string_of_int (pos.start_pos.Lexing.pos_cnum-pos.start_pos.Lexing.pos_bol))^": "

let print s = if !debug_on then (print_string ("\n\n!!!" ^ s); flush stdout) else ()

let pprint msg (pos:loc) = 
  let tmp = pos.start_pos.Lexing.pos_fname ^ ":" ^ (string_of_int pos.start_pos.Lexing.pos_lnum) ^ ": "^ (string_of_int (pos.start_pos.Lexing.pos_cnum-pos.start_pos.Lexing.pos_bol))^ ": " ^ msg in
	print tmp

(* system development debugging *)
let ho_print flag (pr:'a->string) (m:'a) : unit = 
  let d = StackTrace.is_same_dd_get () in
  if flag (* !devel_debug_on *)  || not(d==None) then 
    let s = (pr m) in
    let msg = match d with 
      | None -> ("\n!!!" ^ s)
      | Some cid -> ("\n@"^(string_of_int cid)^"!"^ s) 
    in
    if !log_devel_debug then 
      Buffer.add_string debug_log msg
    else
      (print_string msg; flush stdout)
  else ()

(* system development debugging *)
let devel_print s = 
  ho_print !devel_debug_on (fun x -> x) s 
(* let d = StackTrace.is_same_dd_get () in *)
(*   if !devel_debug_on  || not(d==None) then  *)
(*     let msg = match d with  *)
(*       | None -> ("\n!!!" ^ s) *)
(*       | Some cid -> ("\n@"^(string_of_int cid)^"!"^ s)  *)
(*     in *)
(*     if !log_devel_debug then  *)
(*       Buffer.add_string debug_log msg *)
(*     else *)
(*       (print_string msg; flush stdout) *)
(*   else () *)

(* let prior_msg pos = *)
(*   let tmp = pos.start_pos.Lexing.pos_fname ^ ":" ^ (string_of_int pos.start_pos.Lexing.pos_lnum) ^ ": " ^ (string_of_int (pos.start_pos.Lexing.pos_cnum-pos.start_pos.Lexing.pos_bol)) ^ ": " in *)
(*   let tmp = if is_no_pos !entail_pos then tmp *)
(*   else (tmp^"[entail:"^(string_of_int !entail_pos.start_pos.Lexing.pos_lnum)^"]"^"[post:"^(string_of_int (post_pos#get).start_pos.Lexing.pos_lnum)^"]") *)
(*   in tmp *)

let prior_msg pos = ""

let devel_pprint (msg:string) (pos:loc) =
	ho_print !devel_debug_on (fun m -> (prior_msg pos)^m) msg

let devel_hprint (pr:'a->string) (m:'a) (pos:loc) =
	ho_print !devel_debug_on (fun x -> (prior_msg pos)^(pr x)) m

let devel_zprint msg (pos:loc) =
	ho_print !devel_debug_on (fun m -> (prior_msg pos)^(Lazy.force m)) msg

let dinfo_zprint m p = devel_zprint m p
let dinfo_hprint pr m p  = devel_hprint pr m p
let dinfo_pprint m p = devel_pprint m p

let ninfo_zprint m p = ()
let ninfo_hprint pr m p  = ()
let ninfo_pprint m p = ()

let dinfo_zprint m = devel_zprint m no_pos
let dinfo_hprint pr m  = devel_hprint pr m no_pos
let dinfo_pprint m = devel_pprint m no_pos

let ninfo_zprint m = ()
let ninfo_hprint pr m  = ()
let ninfo_pprint m = ()

let trace_pprint (msg:string) (pos:loc) : unit = 
	ho_print false (fun a -> " "^a) msg

let trace_hprint (pr:'a->string) (m:'a) (pos:loc) = 
	ho_print false (fun x -> " "^(pr x)) m

let trace_zprint m (pos:loc) = 
	ho_print false (fun x -> Lazy.force x) m

let tinfo_zprint m p = trace_zprint m p
let tinfo_hprint pr m p  = trace_hprint pr m p
let tinfo_pprint m p = trace_pprint m p

let info_pprint (msg:string) (pos:loc) : unit = 
	ho_print true (fun a -> " "^a) msg

let info_hprint (pr:'a->string) (m:'a) (pos:loc) = 
	ho_print true (fun x -> " "^(pr x)) m

let info_zprint m (pos:loc) = 
	ho_print true (fun x -> Lazy.force x) m

let info_pprint (msg:string) : unit = info_pprint msg no_pos
let info_hprint (pr:'a->string) (m:'a) = info_hprint pr m no_pos
let info_zprint m (pos:loc) = info_zprint m no_pos

(* let devel_zprint msg (pos:loc) = *)
(* 	lazy_print (prior_msg pos) msg *)

(* let trace_zprint msg (pos:loc) =  *)
(* 	lazy_print (fun () -> " ") msg *)


let print_info prefix str (pos:loc) = 
  let tmp = "\n" ^ prefix ^ ":" ^ pos.start_pos.Lexing.pos_fname ^ ":" ^ (string_of_int pos.start_pos.Lexing.pos_lnum) ^": " ^ (string_of_int (pos.start_pos.Lexing.pos_cnum-pos.start_pos.Lexing.pos_bol)) ^": " ^ str ^ "\n" in
	print_string tmp; flush stdout


open StackTrace
 
  (* let ho_2_opt_aux (loop_d:bool) (test:'z -> bool) (s:string) (pr1:'a->string) (pr2:'b->string) (pr_o:'z->string)  (f:'a -> 'b -> 'z)  *)
  (*       (e1:'a) (e2:'b) : 'z = *)
  (*   let s,h = push s in *)
  (*   (if loop_d then print_string (h^" inp :"^(pr1 e1)^"\n")); *)
  (*   let r = try *)
  (*     pop_ho (f e1) e2  *)
  (*   with ex ->  *)
  (*       let _ = print_string (h^"\n") in *)
  (*       let _ = print_string (s^" inp1 :"^(pr1 e1)^"\n") in *)
  (*       let _ = print_string (s^" inp2 :"^(pr2 e2)^"\n") in *)
  (*       let _ = print_string (s^" Exception"^(Printexc.to_string ex)^"Occurred!\n") in *)
  (*       raise ex in *)
  (*   if not(test r) then r else *)
  (*     let _ = print_string (h^"\n") in *)
  (*     let _ = print_string (s^" inp1 :"^(pr1 e1)^"\n") in *)
  (*     let _ = print_string (s^" inp2 :"^(pr2 e2)^"\n") in *)
  (*     let _ = print_string (s^" out :"^(pr_o r)^"\n") in *)
  (*     r *)

let ho_aux df lz (loop_d:bool) (test:'z -> bool) (g:('a->'z) option) (s:string) (args:string list) (pr_o:'z->string) (f:'a->'z) (e:'a) :'z =
  let pr_args xs =
    let rec helper (i:int) args = match args with
      | [] -> ()
      | a::args -> (print_string (s^" inp"^(string_of_int i)^" :"^a^"\n");(helper (i+1) args)) in
    helper 1 xs in
  let pr_lazy_res xs =
    let rec helper xs = match xs with
      | [] -> ()
      | (i,a)::xs -> let a1=Lazy.force a in
        if (a1=(List.nth args (i-1))) then helper xs
        else (print_string (s^" res"^(string_of_int i)^" :"^(a1)^"\n");(helper xs)) in
    helper xs in
  let (test,pr_o) = match g with
    | None -> (test,pr_o)
    | Some g -> 
          let res = ref (None:(string option)) in
          let new_test z =
            (try
              let r = g e in
              let rs = pr_o r in              
              if String.compare (pr_o z) rs==0 then false
              else (res := Some rs; true)
            with ex ->  
                (res := Some (" OLD COPY : EXIT Exception"^(Printexc.to_string ex)^"!\n");
                true)) in
          let new_pr_o x = (match !res with
            | None -> pr_o x
            | Some s -> ("DIFFERENT RESULT from PREVIOUS METHOD"^
                  ("\n PREV :"^s)^
                  ("\n NOW :"^(pr_o x)))) in
          (new_test, new_pr_o) in
  let s,h = push_call_gen s df in
  (if loop_d then print_string ("\n"^h^" ENTRY :"^(String.concat "  " args)^"\n"));
  flush stdout;
  let r = (try
    pop_aft_apply_with_exc f e
  with ex -> 
      (let _ = print_string ("\n"^h^"\n") in
      (* if not df then *) (pr_args args; pr_lazy_res lz);
      let _ = print_string (s^" EXIT Exception"^(Printexc.to_string ex)^"Occurred!\n") in
      flush stdout;
      raise ex)) in
  (if not(test r) then r else
    let _ = print_string ("\n"^h^"\n") in
    (* if not df then *) (pr_args args; pr_lazy_res lz);
    let _ = print_string (s^" EXIT out :"^(pr_o r)^"\n") in
    flush stdout;
    r)

let choose bs xs = 
  let rec hp bs xs = match bs,xs with
    |[], _ -> []
    | _, [] -> []
    | b::bs, (i,s)::xs -> if b then (i,s)::(hp bs xs) else (hp bs xs) in
  hp bs xs

let ho_aux_no (f:'a -> 'z) (last:'a) : 'z =
  push_no_call ();
  pop_aft_apply_with_exc_no f last


let ho_1_opt_aux df (flags:bool list) (loop_d:bool) (test:'z -> bool) g (s:string) (pr1:'a->string) (pr_o:'z->string)  (f:'a -> 'z) (e1:'a) : 'z =
  let a1 = pr1 e1 in
  let lz = choose flags [(1,lazy (pr1 e1))] in
  let f  = f in
  ho_aux df lz loop_d test g s [a1] pr_o  f  e1


let ho_2_opt_aux df (flags:bool list) (loop_d:bool) (test:'z -> bool) g (s:string) (pr1:'a->string) (pr2:'b->string) (pr_o:'z->string)  (f:'a -> 'b -> 'z) 
      (e1:'a) (e2:'b) : 'z =
  let a1 = pr1 e1 in
  let a2 = pr2 e2 in
  let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2))] in
  let f  = f e1 in
  let g  = match g with None -> None | Some g -> Some (g e1) in
  ho_aux df lz loop_d test g s [a1;a2] pr_o f e2

let ho_3_opt_aux df  (flags:bool list) (loop_d:bool) (test:'z -> bool) g (s:string) (pr1:'a->string) (pr2:'b->string) (pr3:'c->string) (pr_o:'z->string)  (f:'a -> 'b -> 'c -> 'z) (e1:'a) (e2:'b) (e3:'c) : 'z =
  let a1 = pr1 e1 in
  let a2 = pr2 e2 in
  let a3 = pr3 e3 in
  let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2)); (3,lazy (pr3 e3))] in
  let f  = f e1 e2 in
  let g  = match g with None -> None | Some g -> Some (g e1 e2) in
  ho_aux df lz loop_d test g s [a1;a2;a3] pr_o f e3


let ho_4_opt_aux df (flags:bool list) (loop_d:bool) (test:'z->bool) g (s:string) (pr1:'a->string) (pr2:'b->string) (pr3:'c->string) (pr4:'d->string) (pr_o:'z->string) 
      (f:'a -> 'b -> 'c -> 'd-> 'z) (e1:'a) (e2:'b) (e3:'c) (e4:'d): 'z =
  let a1 = pr1 e1 in
  let a2 = pr2 e2 in
  let a3 = pr3 e3 in
  let a4 = pr4 e4 in
  let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2)); (3,lazy (pr3 e3)); (4,lazy (pr4 e4))] in
  let f  = f e1 e2 e3 in
  let g  = match g with None -> None | Some g -> Some (g e1 e2 e3) in
  ho_aux df lz loop_d test g s [a1;a2;a3;a4] pr_o f e4


let ho_5_opt_aux df (flags:bool list) (loop_d:bool) (test:'z -> bool)  g (s:string) (pr1:'a->string) (pr2:'b->string) (pr3:'c->string) (pr4:'d->string)
      (pr5:'e->string) (pr_o:'z->string) 
      (f:'a -> 'b -> 'c -> 'd -> 'e -> 'z) (e1:'a) (e2:'b) (e3:'c) (e4:'d) (e5:'e) : 'z =
  let a1 = pr1 e1 in
  let a2 = pr2 e2 in
  let a3 = pr3 e3 in
  let a4 = pr4 e4 in
  let a5 = pr5 e5 in
  let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2)); (3,lazy (pr3 e3)); (4,lazy (pr4 e4)); (5,lazy (pr5 e5))] in
  let f  = f e1 e2 e3 e4 in
  let g  = match g with None -> None | Some g -> Some (g e1 e2 e3 e4) in
  ho_aux df lz loop_d test g s [a1;a2;a3;a4;a5] pr_o f e5


let ho_6_opt_aux df (flags:bool list) (loop_d:bool) (test:'z->bool) g (s:string) (pr1:'a->string) (pr2:'b->string) (pr3:'c->string) (pr4:'d->string)
      (pr5:'e->string) (pr6:'f->string) (pr_o:'z->string) 
      (f:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'z) (e1:'a) (e2:'b) (e3:'c) (e4:'d) (e5:'e) (e6:'f): 'z =
  let a1 = pr1 e1 in
  let a2 = pr2 e2 in
  let a3 = pr3 e3 in
  let a4 = pr4 e4 in
  let a5 = pr5 e5 in
  let a6 = pr6 e6 in
  let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2)); (3,lazy (pr3 e3)); (4,lazy (pr4 e4)); (5,lazy (pr5 e5)); (6,lazy (pr6 e6))] in
  let f  = f e1 e2 e3 e4 e5 in
  let g  = match g with None -> None | Some g -> Some (g e1 e2 e3 e4 e5) in
  ho_aux df lz loop_d test g s [a1;a2;a3;a4;a5;a6] pr_o f e6

let ho_7_opt_aux df (flags:bool list) (loop_d:bool) (test:'z->bool) g (s:string) (pr1:'a->string) (pr2:'b->string) (pr3:'c->string) (pr4:'d->string)
      (pr5:'e->string) (pr6:'f->string) (pr7:'h->string) (pr_o:'z->string) 
      (f:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'h-> 'z) (e1:'a) (e2:'b) (e3:'c) (e4:'d) (e5:'e) (e6:'f) (e7:'h): 'z =
  let a1 = pr1 e1 in
  let a2 = pr2 e2 in
  let a3 = pr3 e3 in
  let a4 = pr4 e4 in
  let a5 = pr5 e5 in
  let a6 = pr6 e6 in
  let a7 = pr7 e7 in
  let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2)); (3,lazy (pr3 e3)); (4,lazy (pr4 e4)); (5,lazy (pr5 e5)); (6,lazy (pr6 e6)); (7,lazy (pr7 e7))] in
  let f  = f e1 e2 e3 e4 e5 e6 in
  let g  = match g with None -> None | Some g -> Some (g e1 e2 e3 e4 e5 e6) in
  ho_aux df lz loop_d test g s [a1;a2;a3;a4;a5;a6;a7] pr_o f e7

(* better re-organization *)
let ho_1_preopt f b_loop = ho_1_opt_aux false [] b_loop f None
let to_1_preopt f b_loop = ho_1_opt_aux true [] b_loop f None
let ho_1_pre b_loop = ho_1_preopt (fun _ -> true) b_loop
let to_1_pre b_loop = to_1_preopt (fun _ -> true) b_loop
let ho_1 s = ho_1_pre false s
let to_1 s = to_1_pre false s
let ho_1_opt f = ho_1_preopt f false
let ho_1_loop s = ho_1_pre true s 

let ho_1 s = ho_1_opt_aux false [] false (fun _ -> true) None s
let ho_2 s = ho_2_opt_aux false [] false (fun _ -> true) None s
let ho_3 s = ho_3_opt_aux false [] false (fun _ -> true) None s
let ho_4 s = ho_4_opt_aux false [] false (fun _ -> true) None s
let ho_5 s = ho_5_opt_aux false [] false (fun _ -> true) None s
let ho_6 s = ho_6_opt_aux false [] false (fun _ -> true) None s
let ho_7 s = ho_7_opt_aux false [] false (fun _ -> true) None s

let to_1 s = ho_1_opt_aux true [] false (fun _ -> true) None s
let to_2 s = ho_2_opt_aux true [] false (fun _ -> true) None s
let to_3 s = ho_3_opt_aux true [] false (fun _ -> true) None s
let to_4 s = ho_4_opt_aux true [] false (fun _ -> true) None s
let to_5 s = ho_5_opt_aux true [] false (fun _ -> true) None s
let to_6 s = ho_6_opt_aux true [] false (fun _ -> true) None s

let no_1 _ _ _ f 
      = ho_aux_no f
let no_2 _ _ _ _ f e1 
      = ho_aux_no (f e1)
let no_3 _ _ _ _ _ f e1 e2 
      = ho_aux_no (f e1 e2)
let no_4 _ _ _ _ _ _ f e1 e2 e3 
      = ho_aux_no (f e1 e2 e3)
let no_5 _ _ _ _ _ _ _ f e1 e2 e3 e4 
      = ho_aux_no (f e1 e2 e3 e4)
let no_6 _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5 
      = ho_aux_no (f e1 e2 e3 e4 e5)

let no_7 _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5 e6
      = ho_aux_no (f e1 e2 e3 e4 e5 e6)

let ho_1_opt f = ho_1_opt_aux false [] false f None
let ho_2_opt f = ho_2_opt_aux false [] false f None
let ho_3_opt f = ho_3_opt_aux false [] false f None
let ho_4_opt f = ho_4_opt_aux false [] false f None
let ho_5_opt f = ho_5_opt_aux false [] false f None
let ho_6_opt f = ho_6_opt_aux false [] false f None

let to_1_opt f = ho_1_opt_aux true [] false f None
let to_2_opt f = ho_2_opt_aux true [] false f None
let to_3_opt f = ho_3_opt_aux true [] false f None
let to_4_opt f = ho_4_opt_aux true [] false f None
let to_5_opt f = ho_5_opt_aux true [] false f None
let to_6_opt f = ho_6_opt_aux true [] false f None

let no_1_opt _ _ _ _ f 
      = ho_aux_no f
let no_2_opt _ _ _ _ _ f e1 
      = ho_aux_no (f e1)
let no_3_opt _ _ _ _ _ _ f e1 e2 
      = ho_aux_no (f e1 e2)
let no_4_opt _ _ _ _ _ _ _ f e1 e2 e3 
      = ho_aux_no (f e1 e2 e3)
let no_5_opt _ _ _ _ _ _ _ _ f e1 e2 e3 e4 
      = ho_aux_no (f e1 e2 e3 e4)
let no_6_opt _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5 
      = ho_aux_no (f e1 e2 e3 e4 e5)

let add_num f i s = let str=(s^"#"^(string_of_int i)) in f str

let ho_1_num i =  add_num ho_1 i
let ho_2_num i =  add_num ho_2 i
let ho_3_num i =  add_num ho_3 i
let ho_4_num i =  add_num ho_4 i
let ho_5_num i =  add_num ho_5 i
let ho_6_num i =  add_num ho_6 i

let to_1_num i =  add_num to_1 i
let to_2_num i =  add_num to_2 i
let to_3_num i =  add_num to_3 i
let to_4_num i =  add_num to_4 i
let to_5_num i =  add_num to_5 i
let to_6_num i =  add_num to_6 i

let no_1_num (i:int) s _ _ f
      = ho_aux_no f
let no_2_num (i:int) s _ _ _ f e1
      = ho_aux_no (f e1)
let no_3_num (i:int) s _ _ _ _ f e1 e2
      = ho_aux_no (f e1 e2)
let no_4_num (i:int) s _ _ _ _ _ f e1 e2 e3
      = ho_aux_no (f e1 e2 e3)
let no_5_num (i:int) s _ _ _ _ _ _ f e1 e2 e3 e4
      = ho_aux_no (f e1 e2 e3 e4)
let no_6_num (i:int) s _ _ _ _ _ _ _ f e1 e2 e3 e4 e5
      = ho_aux_no (f e1 e2 e3 e4 e5)

let ho_1_cmp g = ho_1_opt_aux false [] false (fun _ -> true) (Some g) 
let ho_2_cmp g = ho_2_opt_aux false [] false (fun _ -> true) (Some g) 
let ho_3_cmp g = ho_3_opt_aux false [] false (fun _ -> true) (Some g) 
let ho_4_cmp g = ho_4_opt_aux false [] false (fun _ -> true) (Some g) 
let ho_5_cmp g = ho_5_opt_aux false [] false (fun _ -> true) (Some g) 
let ho_6_cmp g = ho_6_opt_aux false [] false (fun _ -> true) (Some g) 

let no_1_cmp _ _ _ _ f 
      = ho_aux_no f
let no_2_cmp _ _ _ _ _ f e1 
      = ho_aux_no (f e1)
let no_3_cmp _ _ _ _ _ _ f e1 e2 
      = ho_aux_no (f e1 e2)
let no_4_cmp _ _ _ _ _ _ _ f e1 e2 e3 
      = ho_aux_no (f e1 e2 e3)
let no_5_cmp _ _ _ _ _ _ _ _ f e1 e2 e3 e4 
      = ho_aux_no (f e1 e2 e3 e4)
let no_6_cmp _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5 
      = ho_aux_no (f e1 e2 e3 e4 e5)

let ho_eff_1 s l = ho_1_opt_aux false l false (fun _ -> true) None s
let ho_eff_2 s l = ho_2_opt_aux false l false (fun _ -> true) None s
let ho_eff_3 s l = ho_3_opt_aux false l false (fun _ -> true) None s
let ho_eff_4 s l = ho_4_opt_aux false l false (fun _ -> true) None s
let ho_eff_5 s l = ho_5_opt_aux false l false (fun _ -> true) None s
let ho_eff_6 s l = ho_6_opt_aux false l false (fun _ -> true) None s

let to_eff_1 s l = ho_1_opt_aux true l false (fun _ -> true) None s
let to_eff_2 s l = ho_2_opt_aux true l false (fun _ -> true) None s
let to_eff_3 s l = ho_3_opt_aux true l false (fun _ -> true) None s
let to_eff_4 s l = ho_4_opt_aux true l false (fun _ -> true) None s
let to_eff_5 s l = ho_5_opt_aux true l false (fun _ -> true) None s
let to_eff_6 s l = ho_6_opt_aux true l false (fun _ -> true) None s

let no_eff_1 _ _ _ _ f 
      = ho_aux_no f
let no_eff_2 _ _ _ _ _ f e1 
      = ho_aux_no (f e1)
let no_eff_3 _ _ _ _ _ _ f e1 e2 
      = ho_aux_no (f e1 e2)
let no_eff_4 _ _ _ _ _ _ _ f e1 e2 e3 
      = ho_aux_no (f e1 e2 e3)
let no_eff_5 _ _ _ _ _ _ _ _ f e1 e2 e3 e4 
      = ho_aux_no (f e1 e2 e3 e4)
let no_eff_6 _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5 
      = ho_aux_no (f e1 e2 e3 e4 e5)

let ho_eff_1_num i =  add_num ho_eff_1 i
let ho_eff_2_num i =  add_num ho_eff_2 i
let ho_eff_3_num i =  add_num ho_eff_3 i
let ho_eff_4_num i =  add_num ho_eff_4 i
let ho_eff_5_num i =  add_num ho_eff_5 i
let ho_eff_6_num i =  add_num ho_eff_6 i

let to_eff_1_num i =  add_num to_eff_1 i
let to_eff_2_num i =  add_num to_eff_2 i
let to_eff_3_num i =  add_num to_eff_3 i
let to_eff_4_num i =  add_num to_eff_4 i
let to_eff_5_num i =  add_num to_eff_5 i
let to_eff_6_num i =  add_num to_eff_6 i

let no_eff_1_num _ _ _ _ _ f 
      =  ho_aux_no (f)
let no_eff_2_num _ _ _ _ _ _ f e1 
      =  ho_aux_no (f e1)
let no_eff_3_num _ _ _ _ _ _ _ f e1 e2 
      =  ho_aux_no (f e1 e2)
let no_eff_4_num _ _ _ _ _ _ _ _ f e1 e2 e3 
      =  ho_aux_no (f e1 e2 e3)
let no_eff_5_num _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 
      =  ho_aux_no (f e1 e2 e3 e4)
let no_eff_6_num _ _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5 
      =  ho_aux_no (f e1 e2 e3 e4 e5)

let to_1_loop s = ho_1_opt_aux true [] true (fun _ -> true) None s
let to_2_loop s = ho_2_opt_aux true [] true (fun _ -> true) None s
let to_3_loop s = ho_3_opt_aux true [] true (fun _ -> true) None s
let to_4_loop s = ho_4_opt_aux true [] true (fun _ -> true) None s
let to_5_loop s = ho_5_opt_aux true [] true (fun _ -> true) None s
let to_6_loop s = ho_6_opt_aux true [] true (fun _ -> true) None s

let ho_1_loop s = ho_1_opt_aux false [] true (fun _ -> true) None s
let ho_2_loop s = ho_2_opt_aux false [] true (fun _ -> true) None s
let ho_3_loop s = ho_3_opt_aux false [] true (fun _ -> true) None s
let ho_4_loop s = ho_4_opt_aux false [] true (fun _ -> true) None s
let ho_5_loop s = ho_5_opt_aux false [] true (fun _ -> true) None s
let ho_6_loop s = ho_6_opt_aux false [] true (fun _ -> true) None s


let no_1_loop _ _ _ f 
      = ho_aux_no f
let no_2_loop _ _ _ _ f e1 
      = ho_aux_no (f e1)
let no_3_loop _ _ _ _ _ f e1 e2
      = ho_aux_no (f e1 e2)
let no_4_loop _ _ _ _ _ _ f e1 e2 e3
      = ho_aux_no (f e1 e2 e3)
let no_5_loop _ _ _ _ _ _ _ f e1 e2 e3 e4
      = ho_aux_no (f e1 e2 e3 e4)
let no_6_loop _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5
      = ho_aux_no (f e1 e2 e3 e4 e5)

let ho_1_loop_num i =  add_num ho_1_loop i
let ho_2_loop_num i =  add_num ho_2_loop i
let ho_3_loop_num i =  add_num ho_3_loop i
let ho_4_loop_num i =  add_num ho_4_loop i
let ho_5_loop_num i =  add_num ho_5_loop i
let ho_6_loop_num i =  add_num ho_6_loop i

let to_1_loop_num i =  add_num to_1_loop i
let to_2_loop_num i =  add_num to_2_loop i
let to_3_loop_num i =  add_num to_3_loop i
let to_4_loop_num i =  add_num to_4_loop i
let to_5_loop_num i =  add_num to_5_loop i
let to_6_loop_num i =  add_num to_6_loop i

let no_1_loop_num _ _ _ _ f 
      = ho_aux_no f
let no_2_loop_num _ _ _ _ _ f e1 
      = ho_aux_no (f e1)
let no_3_loop_num _ _ _ _ _ _ f e1 e2
      = ho_aux_no (f e1 e2)
let no_4_loop_num _ _ _ _ _ _ _ f e1 e2 e3
      = ho_aux_no (f e1 e2 e3)
let no_5_loop_num _ _ _ _ _ _ _ _ f e1 e2 e3 e4
      = ho_aux_no (f e1 e2 e3 e4)
let no_6_loop_num _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5
      = ho_aux_no (f e1 e2 e3 e4 e5)

  (* let no_eff_1_opt  _ _ _ _ _ f = f *)
  (* let no_eff_2_opt  _ _ _ _ _ _ f = f *)
  (* let no_eff_3_opt  _ _ _ _ _ _ _ f = f *)
  (* let no_eff_4_opt  _ _ _ _ _ _ _ _ f = f *)
  (* let no_eff_5_opt  _ _ _ _ _ _ _ _ _ f = f *)
  (* let no_eff_6_opt  _ _ _ _ _ _ _ _ _ _ f = f *)
