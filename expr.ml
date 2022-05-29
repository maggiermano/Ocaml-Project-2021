(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Divide 
  | Equals
  | LessThan
  | GreaterThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float                       (* floats *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;; 
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var v -> SS.add v SS.empty 
  | Num _ | Float _ | Bool _ | Raise | Unassigned -> SS.empty
  | Unop (_, ex) -> free_vars ex 
  | Binop (_, ex, exp) | App (ex, exp) -> SS.union (free_vars ex) (free_vars exp)
  | Conditional (ex, exp, expr) -> SS.union (free_vars ex) (SS.union (free_vars exp) (free_vars expr))
  | Fun (v, ex) -> SS.remove v (free_vars ex)
  | Let (v, ex, exp) -> SS.remove v (SS.union (free_vars ex) (free_vars exp))
  | Letrec (v, ex, exp) -> SS.union (SS.remove v (free_vars ex)) (SS.remove v (free_vars exp)) ;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname : unit -> varid =
  let v = ref 0 in
  fun () -> 
   v := !v + 1;
   "v" ^ string_of_int (!v - 1) ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let subst_helper = subst var_name repl in 
  match exp with
  | Var v -> if v = var_name then repl else exp 
  | Num _ | Float _ | Bool _ | Raise | Unassigned -> exp 
  | Unop (un, e) -> Unop (un, subst_helper e)
  | Binop (bi, e, ex) -> Binop (bi, subst_helper e, subst_helper ex)
  | Conditional (e, ex, exx) -> Conditional (subst_helper e, subst_helper ex, subst_helper exx)
  | Fun (v, e) -> 
    if v = var_name then exp 
    else if SS.mem v (free_vars repl) then let v' = new_varname () in
      Fun (v', subst_helper (subst v (Var v') e))
    else Fun (v, subst_helper e)
  | Let (v, e, ex) -> 
      if v = var_name then Let (v, subst_helper e, ex)
      else if SS.mem v (free_vars repl) then let v' = new_varname () in 
        Let (v', subst_helper e, subst_helper (subst v (Var v') ex))
      else Let (v, subst_helper e, subst_helper ex)
  | Letrec (v, e, ex) -> 
    if v = var_name then exp 
    else if SS.mem v (free_vars repl) then let v' = new_varname () in 
      Letrec (v', subst_helper (subst v (Var v') e), subst_helper (subst v (Var v') ex))
    else Letrec (v, subst_helper e, subst_helper ex)
  | App (e, ex) -> App (subst_helper e, subst_helper ex) ;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var v -> v
  | Num num -> string_of_int num
  | Float flo -> string_of_float flo
  | Bool bool -> string_of_bool bool
  | Unop (un, ex) ->
    (match un with
     | Negate -> "~- " ^ exp_to_concrete_string ex)
  | Binop (bi, ex, exp) ->
    let make_str bis = exp_to_concrete_string ex ^ bis ^
                        exp_to_concrete_string exp in
    (match bi with
      | Plus -> make_str " + "
      | Minus -> make_str " - "
      | Times -> make_str " * "
      | Divide -> make_str " / "
      | Equals -> make_str " = "
      | LessThan -> make_str " < "
      | GreaterThan -> make_str " > ")
  | Conditional (ex, exp, expr) ->
    "if " ^ (exp_to_concrete_string ex) ^ " then " ^
    (exp_to_concrete_string exp) ^ " else " ^ (exp_to_concrete_string expr)
  | Fun (v, ex) -> "fun " ^ v ^ " -> " ^ exp_to_concrete_string ex
  | Let (v, ex, exp) ->
    "let " ^ v ^ " = " ^ exp_to_concrete_string ex ^ " in " ^
    exp_to_concrete_string exp
  | Letrec (v, ex, exp) ->
    "let rec " ^ v ^ " = " ^ exp_to_concrete_string ex ^ " in " ^
    exp_to_concrete_string exp
  | Raise -> "Exception"
  | Unassigned -> "Unassigned"
  | App (ex, exp) ->
    exp_to_concrete_string ex ^ " " ^ exp_to_concrete_string exp ;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var v -> "Var(" ^ v ^ ")"
  | Num num -> "Num(" ^ string_of_int num ^ ")"
  | Float flo -> "Float(" ^ string_of_float flo ^ ")"
  | Bool bool -> "Bool(" ^ string_of_bool bool ^ ")"
  | Unop (un, ex) ->
    (match un with
     | Negate -> "Unop(Negate, " ^ exp_to_abstract_string ex ^ ")")
  | Binop (bi, ex, exp) ->
    let make_str op_s = "Binop(" ^ op_s ^ ", " ^  exp_to_abstract_string ex ^
                        ", " ^ exp_to_abstract_string exp ^ ")" in
    (match bi with
      | Plus -> make_str "Plus"
      | Minus -> make_str "Minus"
      | Times -> make_str "Times"
      | Divide -> make_str "Divide"
      | Equals -> make_str "Equals"
      | LessThan -> make_str "LessThan"
      | GreaterThan -> make_str "GreaterThan")
  | Conditional (ex, exp, expr) ->
    "Conditional(" ^ (exp_to_abstract_string ex) ^ ", " ^
    (exp_to_abstract_string exp) ^ ", " ^ (exp_to_abstract_string expr) ^ ")"
  | Fun (v, ex) -> "Fun(" ^ v ^ ", " ^ exp_to_abstract_string ex ^ ")"
  | Let (v, ex, exp) ->
    "Let(" ^ v ^ ", " ^ exp_to_abstract_string ex ^ ", " ^
    exp_to_abstract_string exp ^ ")"
  | Letrec (v, ex, exp) ->
    "Letrec(" ^ v ^ ", " ^ exp_to_abstract_string ex ^ ", " ^
    exp_to_abstract_string exp ^ ")"
  | Raise -> "Exception"
  | Unassigned -> "Unassigned"
  | App (ex, exp) ->
    "App(" ^ exp_to_abstract_string ex ^ ", " ^ exp_to_abstract_string exp ^ ")" ;;
