(*
													CS51 Final Project
											Unit Testing for Evals
*)

open Expr ;;
open Evaluation ;;

let unit_test (condition : bool) (msg : string) : unit =
  if condition then
    Printf.printf "%s passed\n" msg
  else Printf.printf "%s FAILED\n" msg ;;

let exprtest () =
  unit_test (exp_to_abstract_string (Num (1)) = "Num(1)") "e_to_ab_s num";
  unit_test (exp_to_abstract_string (Bool (true)) = "Bool(true)") "e_to_ab_s bool true";
  unit_test (exp_to_abstract_string (Var ("x")) = "Var(x)") "e_to_ab_s var";
  unit_test (exp_to_abstract_string (Unop(Negate, Num(1))) = "Unop(Negate, Num(1))") "e_to_ab_s negate";
  unit_test (exp_to_abstract_string (Binop(Plus, Num(1), Var("x"))) = "Binop(Plus, Num(1), Var(x))") "e_to_ab_s plus";
  unit_test (exp_to_abstract_string (Binop(Equals, Num(1), Var("x"))) = "Binop(Equals, Num(1), Var(x))") "e_to_ab_s equals";
  unit_test (exp_to_abstract_string (Binop(LessThan, Num(1), Var("x"))) = "Binop(LessThan, Num(1), Var(x))") "e_to_ab_s less than";
  unit_test (exp_to_abstract_string (Binop(Minus, Num(3), Var("x"))) = "Binop(Minus, Num(3), Var(x))") "e_to_ab_s minus";
  unit_test (exp_to_abstract_string (App(Fun("x", Var("x")), Num(5))) = "App(Fun(x, Var(x)), Num(5))") "e_to_ab_s app1";
  unit_test (exp_to_abstract_string (App(Fun("x", Binop(Plus, Var("x"), Var("z"))), Var("y"))) = "App(Fun(x, Binop(Plus, Var(x), Var(z))), Var(y))") "e_to_ab_s app2";
  unit_test (exp_to_abstract_string (Fun("x", Num(5))) = "Fun(x, Num(5))") "e_to_ab_s fun";
  unit_test (exp_to_abstract_string (Let("a", Bool(true), Num(8))) = "Let(a, Bool(true), Num(8))") "e_to_ab_s let";
  unit_test (exp_to_abstract_string (Letrec("x", Fun("x", Var("x")), Num(8))) = "Letrec(x, Fun(x, Var(x)), Num(8))") "e_to_ab_s let rec";
  unit_test (exp_to_abstract_string (Conditional(Bool(true), Var("x"), Var("y"))) = "Conditional(Bool(true), Var(x), Var(y))") "e_to_ab_s conditional";

  unit_test (same_vars (free_vars (Num (1))) (vars_of_list [])) "free_vars num";
  unit_test (same_vars (free_vars (Bool (true))) (vars_of_list [])) "free_vars bool true";
  unit_test (same_vars (free_vars (Bool (false))) (vars_of_list [])) "free_vars bool false";
  unit_test (same_vars (free_vars (Var ("x"))) (vars_of_list ["x"])) "free_vars var";
  unit_test (same_vars (free_vars (Binop(Plus, Num(1), Var("x")))) (vars_of_list ["x"])) "free_vars binop 1 + x";
  unit_test (same_vars (free_vars (Unop(Negate, Num(1)))) (vars_of_list [])) "free_vars unop none";
  unit_test (same_vars (free_vars (Unop(Negate, Var("y")))) (vars_of_list ["y"])) "free_vars unop y";
  unit_test (same_vars (free_vars (Binop(Plus, Var("y"), Var("x")))) (vars_of_list ["x";"y"])) "free_vars binop x + y";
  unit_test (same_vars (free_vars (Binop(Plus, Var("y"), Var("x")))) (vars_of_list ["x";"y"])) "free_vars binop x + y";
  unit_test (same_vars (free_vars (Binop(Equals, Var("y"), Var("x")))) (vars_of_list ["x";"y"])) "free_vars binop x = y";
  unit_test (same_vars (free_vars (Binop(Equals, Num(1), Num(3)))) (vars_of_list [])) "free_vars binop 1 = 3";
  unit_test (same_vars (free_vars (App(Fun("x", Var("x")), Num(5)))) (vars_of_list [])) "free_vars app (fun x -> x) 5";
  unit_test (same_vars (free_vars (App(Fun("x", Var("x")), Var("x")))) (vars_of_list ["x"])) "free_vars app (fun x -> x) x";
  unit_test (same_vars (free_vars (App(Fun("x", Var("x")), Var("y")))) (vars_of_list ["y"])) "free_vars app (fun x -> x) y";
  unit_test (same_vars (free_vars (App(Fun("x", Binop(Plus, Var("x"), Num(1))), Var("y")))) (vars_of_list ["y"])) "free_vars app (fun x -> x + 1) y";
  unit_test (same_vars (free_vars (App(Fun("x", Binop(Plus, Var("x"), Var("z"))), Var("y")))) (vars_of_list ["y"; "z"])) "free_vars app (fun x -> x + z) y";
  unit_test (same_vars (free_vars (App(Fun("x", Num(5)), Var("y")))) (vars_of_list ["y"])) "free_vars app (fun x -> 5) y";
  unit_test (same_vars (free_vars (Fun("x", Num(5)))) (vars_of_list [])) "free_vars fun x -> 5";
  unit_test (same_vars (free_vars (Fun("x", Var("x")))) (vars_of_list [])) "free_vars fun x -> x";
  unit_test (same_vars (free_vars (Fun("x", Var("y")))) (vars_of_list ["y"])) "free_vars fun x -> y";
  unit_test (same_vars (free_vars (Fun("y", Binop(Plus, Var("y"), Var("x"))))) (vars_of_list ["x"])) "free_vars fun y -> y + x";
  unit_test (same_vars (free_vars (Fun("y", Binop(Plus, Var("y"), Num(5))))) (vars_of_list [])) "free_vars fun y -> y + 5";
  unit_test (same_vars (free_vars (Let("y", Binop(Plus, Var("x"), Num(5)), Binop(Plus, Var("y"), Var("x"))))) (vars_of_list ["x"])) "free_vars let y = x + 5 in x + y";
  unit_test (same_vars (free_vars (Let("y", Binop(Plus, Var("x"), Num(5)), Binop(Plus, Var("y"), Num(5))))) (vars_of_list ["x"])) "free_vars let y = x + 5 in y + 5";
  unit_test (same_vars (free_vars (Let("y", Num(5), Binop(Plus, Var("y"), Num(5))))) (vars_of_list [])) "free_vars let y = 5 in y + 5";
  unit_test (same_vars (free_vars (Let("y", Num(5), Binop(Plus, Var("y"), Var("x"))))) (vars_of_list ["x"])) "free_vars let y = 5 in y + x";

  unit_test (new_varname () = "v0") "new_varname 1";
  unit_test (new_varname () = "v1") "new_varname 2";

  unit_test ((subst "x" (Num(5)) (Num(1))) = Num(1)) "subst x 1 1";
  unit_test ((subst "y" (Num(1)) (Num(1))) = Num(1)) "subst y 5 1";
  unit_test ((subst "x" (Num(5)) (Var("x"))) = Num(5)) "subst x 5 x";
  unit_test ((subst "x" (Num(5)) (Var("y"))) = Var("y")) "subst x 5 y";
  unit_test ((subst "x" (Num(5)) (Var("y"))) = Var("y")) "subst x 5 y";
  unit_test ((subst "y" (Var("x")) (Var("y"))) = Var("x")) "subst y x y";
  unit_test ((subst "y" (Binop(Plus, Var("a"), Var("b"))) (Var("y"))) = (Binop(Plus, Var("a"), Var("b")))) "subst y (a + b) y";
  unit_test ((subst "y"  (Var("x")) (Binop(Plus, Var("a"), Var("b")))) = Binop(Plus, Var("a"), Var("b"))) "subst y x (a + b)";
  unit_test ((subst "x"  (Var("y")) (Binop(Plus, Var("x"), Var("x")))) = Binop(Plus, Var("y"), Var("y"))) "subst x y (x + x)";
  unit_test ((subst "x"  (Var("y")) (Binop(Minus, Binop(Plus, Num(5), Var("x")), Var("x")))) = Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("y"))) "subst x y (5*x - x)";
  unit_test ((subst "x"  (Var("y")) (Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("x")), Var("x"))))) = Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("x")), Var("x")))) "subst x y (fun x -> 5*x - x)";
  unit_test ((subst "x"  (Var("z")) (Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("y"))))) = Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("y")))) "subst x z (fun x -> 5*y - y)";
  unit_test ((subst "x"  (Var("z")) (Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("x"))))) = Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("x")))) "subst x z (fun x -> 5*y - x)";
  unit_test ((subst "x"  (Var("z")) (Fun("a", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("x"))))) = Fun("a", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("z")))) "subst x z (fun a -> 5*y - x)";
  unit_test ((subst "x" (Num(8)) (Fun("y", Var("x")))) = Fun("y", Num(8))) "subst x 8 (fun y -> x)";
  unit_test ((subst "x" (Var("y")) (Fun("y", Var("x")))) = Fun("v2", Var("y"))) "subst x y (fun y -> x)";
  unit_test ((subst "x" (Var("y")) (Fun("y", Binop(Plus, Var("x"), Var("y"))))) = Fun("v3", Binop(Plus, Var("y"), Var("v3")))) "subst x y (fun y -> x + y)";
  unit_test ((subst "x" (Var("y")) (Let("x", Num(8), Var("x")))) = Let("x", Num(8), Var("x"))) "subst x y (let x = 8 in x)";
  unit_test ((subst "x" (Var("y")) (Let("x", Var("x"), Var("x")))) = Let("x", Var("y"), Var("x"))) "subst x y (let x = x in x)";
  unit_test ((subst "x" (Var("m")) (Let("y", Var("x"), Var("x")))) = Let("y", Var("m"), Var("m"))) "subst x m (let y = x in x)";
  unit_test ((subst "x" (Var("y")) (Let("y", Var("x"), Var("y")))) = Let("v4", Var("y"), Var("v4"))) "subst x y (let y = x in y)";
  unit_test ((subst "x" (Var("y")) (Let("y", Var("x"), Binop(Plus, Var("y"), Var("x"))))) = Let("v5", Var("y"), Binop(Plus, Var("v5"), Var("y")))) "subst x y (let y = x in y + x)";
  unit_test ((subst "x" (Var("y")) (Letrec("x", Binop(Plus, Var("x"), Num(8)), Var("x")))) = Letrec("x", Binop(Plus, Var("x"), Num(8)), Var("x"))) "subst x y (let rec x = x + 8 in x)";
  unit_test ((subst "x" (Var("m")) (Letrec("y", Binop(Plus, Var("x"), Var("y")), Var("x")))) = Letrec("y", Binop(Plus, Var("m"), Var("y")), Var("m"))) "subst x m (let rec y = x + y in x)";
  unit_test ((subst "x" (Var("y")) (Letrec("y", Binop(Plus, Var("x"), Var("y")), Var("y")))) = Letrec("v6", Binop(Plus, Var("y"), Var("v6")), Var("v6"))) "subst x y (let rec y = x + y in y)";
  unit_test ((subst "x" (Var("y")) (Letrec("y", Binop(Plus, Var("x"), Var("y")), Binop(Plus, Var("x"), Var("y"))))) = Letrec("v7", Binop(Plus,Var("y"),Var("v7")), Binop(Plus,Var("y"), Var("v7")) )) "subst x y (let rec y = x + y in x + y)";
  unit_test ((subst "x" (Var("y")) (App(Fun("x", Binop(Plus, Var("x"), Num(1))), Num(5)))) = App(Fun("x", Binop(Plus, Var("x"), Num(1))), Num(5))) "subst x y ((fun x -> x + 1) 5)";
  unit_test ((subst "x" (Var("y")) (App(Fun("a", Binop(Plus, Var("x"), Num(1))), Num(5)))) = App(Fun("a", Binop(Plus, Var("y"), Num(1))), Num(5))) "subst x y ((fun a -> x + 1) 5)";
  unit_test ((subst "x" (Var("y")) (App(Fun("a", Binop(Plus, Var("x"), Num(1))), Var("x")))) = App(Fun("a", Binop(Plus, Var("y"), Num(1))), Var("y"))) "subst x y ((fun a -> x + 1) x)";
  unit_test ((subst "x" (Var("y")) (Conditional(Bool(true), Var("x"), Var("y")))) = Conditional(Bool(true), Var("y"), Var("y"))) "subst x y if true then x else y";
  unit_test ((subst "x" (Var("y")) (Conditional(Binop(Equals, Var("x"), Var("y")), Var("x"), Var("y")))) = Conditional(Binop(Equals, Var("y"), Var("y")), Var("y"), Var("y"))) "subst x y if x = y then x else y";
  unit_test ((subst "x" (Var("y")) (Conditional(Bool(true), Num(5), Num(4)))) = Conditional(Bool(true), Num(5), Num(4))) "subst x y if true then 5 else 4";;

let env = Env.empty () ;;
let close_test () =
  unit_test (Env.close (Num 3) env = Closure (Num 3, env)) "close num";
  unit_test (Env.close (Var "x") env = Closure (Var "x", env)) "close var";
  unit_test (Env.close (Bool true) env = Closure (Bool true, env)) "close bool";
  unit_test (Env.close (Unop (Negate, Var "x")) env = Closure (Unop (Negate, Var "x"), env)) "close unop";
  unit_test (Env.close (Binop (Plus, Var "x", Num 3)) env = Closure (Binop (Plus, Var "x", Num 3), env)) "close binop";
  unit_test (Env.close (Conditional (Binop (Equals, Var "x", Num 5), Var "x", Var "y")) env 
             = Closure (Conditional (Binop (Equals, Var "x", Num 5), Var "x", Var "y"), env)) "close binop";
  unit_test (Env.close (Fun ("x", Binop (Plus, Var "x", Num 3))) env 
             = Closure (Fun ("x", Binop (Plus, Var "x", Num 3)), env)) "close fun";
  unit_test (Env.close (Let ("x", Binop (Plus, Var "y", Num 3), Binop (Plus, Var "x", Num 5))) env 
             = Closure (Let ("x", Binop (Plus, Var "y", Num 3), Binop (Plus, Var "x", Num 5)), env)) "close let";
  unit_test (Env.close (Letrec ("x", Binop (Plus, Var "y", Num 3), Binop (Plus, Var "x", Num 5))) env 
             = Closure (Letrec ("x", Binop (Plus, Var "y", Num 3), Binop (Plus, Var "x", Num 5)), env)) "close letrec";
  unit_test (Env.close Raise env = Closure (Raise, env)) "close raise";
  unit_test (Env.close (App (Fun ("x", Binop (Plus, Var "x", Num 3)), Num 5)) env
             = Closure (App (Fun ("x", Binop (Plus, Var "x", Num 3)), Num 5), env)) "close app" ;;

let lookup_test () =
 let new_env = Env.extend env "x" (ref (Env.Val (Num 5))) in
 unit_test (Env.lookup new_env "x" = Env.Val (Num 5)) "lookup num";;

let env = Env.empty () ;;
let eval_s_test () =
  unit_test (eval_s (Num 0) env = Env.Val (Num 0)) "eval_s num";
  unit_test (eval_s (Bool true) env = Env.Val (Bool true)) "eval_s bool";
  unit_test (eval_s (Unop (Negate, Num 5)) env = Env.Val (Num (~-5))) "eval_s unop num";
  unit_test (eval_s (Binop (Plus, Num 2, Num 3)) env = Env.Val (Num 5)) "eval_s binop plus";
  unit_test (eval_s (Binop (LessThan, Num 1, Num 2)) env = Env.Val (Bool true)) "eval_s binop lessthan";
  unit_test (eval_s (Conditional (Bool true, Num 0, Num 1)) env = Env.Val (Num 0)) "eval_s conditional true";
  unit_test (eval_s (Conditional (Bool false, Num 0, Num 1)) env = Env.Val (Num 1)) "eval_s conditional false";
  unit_test (eval_s (Let ("x", Num 5, Binop(Times, Var "x", Num 3))) env = Env.Val (Num 15)) "eval_s let";
  unit_test (eval_s (Let ("f", Fun("x", Var "x"), App(Var "f", App(Var "f", Num 3)))) env = Env.Val (Num 3)) "eval_s let fun app";
  unit_test (eval_s (Letrec ("f", Fun("x", Conditional(Binop(Equals, Var "x", Num 0), Num 1, Binop(Times, Var "x", App(Var "f", 
                     Binop(Minus, Var "x", Num 1))))), App(Var "f", Num 4))) env = Env.Val (Num 24)) "eval_s letrec";
  unit_test (eval_s (Fun ("x", Var "x")) env = Env.Val (Fun ("x", Var "x"))) "eval_s fun" ;;

let env2 = Env.extend env "x" (ref (Env.Val (Num 5))) ;;
let eval_d_test () =
  unit_test (eval_d (Var "x") env2 = Env.Val (Num 5)) "eval_d Var";
  unit_test (eval_d (Num 0) env2 = Env.Val (Num 0)) "eval_d Num";
  unit_test (eval_d (Bool true) env = Env.Val (Bool true)) "eval_d bool";
  unit_test (eval_d (Unop (Negate, Num 5)) env = Env.Val (Num (~-5))) "eval_d unop num";
  unit_test (eval_d (Binop (Plus, Num 2, Num 3)) env = Env.Val (Num 5)) "eval_d Binop plus";
  unit_test (eval_d (Binop (LessThan, Num 1, Num 2)) env = Env.Val (Bool true)) "eval_d Binop LessThan";
  unit_test (eval_d (Conditional (Bool true, Num 0, Num 1)) env = Env.Val (Num 0)) "eval_d Conditional True";
  unit_test (eval_d (Conditional (Bool false, Num 0, Num 1)) env = Env.Val (Num 1)) "eval_d Conditional False";
  unit_test (eval_d (Let ("f", Fun("x", Var "x"), App(Var "f", App(Var "f", Num 3)))) env = Env.Val (Num 3)) "eval_d Let,Fun,App";
  unit_test (eval_d (Letrec ("f", Fun ("x", Conditional (Binop (Equals, Var "x", Num 0), Num 1, Binop (Times, Var "x", App (Var "f", 
                     Binop (Minus, Var "x", Num 1))))), App (Var "f", Num 4))) env = Env.Val (Num 24)) "eval_d Letrec";
  unit_test (eval_d (Let ("x", Num 1, Let ("f", Fun ("y", Binop (Plus, Var"x", Var"y")), Let ("x", Num 2, App (Var"f", Num 3))))) env
                    = Env.Val (Num 5)) "eval_d Let";
  unit_test (eval_d (Fun ("x", Var "x")) env = Env.Val (Fun ("x", Var "x"))) "eval_d Fun";
  unit_test (eval_d (Fun ("x", Binop (Plus, Var "x", Num 2))) env2 
                     = Env.Val (Fun ("x", Binop (Plus, Var "x", Num 2)))) "eval_d Fun extended env";
  unit_test (eval_d (Let ("x", Num 2, Let ("f", Fun ("y", Binop (Plus, Var"x", Var"y")), Let ("x", Num 8, App (Var"f", Var"x"))))) env
                     = Env.Val (Num 16)) "eval_d let diff" ;;
let eval_l_test () =
  unit_test (eval_l (Num 0) env = Env.Val (Num 0)) "eval_l num";
  unit_test (eval_l (Bool true) env = Env.Val (Bool true)) "eval_l bool";
  let env1 = Env.extend env "x" (ref (Env.Val (Num 2))) in
  unit_test (eval_l (Var "x") env1 = Env.Val (Num 2)) "eval_l var";
  unit_test (eval_l (Unop (Negate, Num 5)) env = Env.Val (Num (~-5))) "eval_l unop num";
  unit_test (eval_l (Binop (Plus, Num 2, Num 3)) env = Env.Val (Num 5)) "eval_l binop plus";
  unit_test (eval_l (Binop (LessThan, Num 1, Num 2)) env = Env.Val (Bool true)) "eval_l binop lessthan";
  unit_test (eval_l (Conditional (Bool true, Num 0, Num 1)) env = Env.Val (Num 0)) "eval_l conditional true";
  unit_test (eval_l (Conditional (Bool false, Num 0, Num 1)) env = Env.Val (Num 1)) "eval_l conditional false";
  unit_test (eval_l (Let ("x", Num 5, Binop(Times, Var "x", Num 3))) env = Env.Val (Num 15)) "eval_l let";
  unit_test (eval_l (Let ("x", Num 1, Let ("f", Fun ("y", Binop (Plus, Var "x", Var "y")), Let ("x", Num 2, App (Var "f", Num 3))))) env
                     = Env.Val (Num 4)) "eval_l let defined";
  unit_test (eval_l (Let ("f", Fun("x", Var "x"), App(Var "f", App(Var "f", Num 3)))) env = Env.Val (Num 3)) "eval_l let fun app";
  unit_test (eval_l (Letrec ("f", Fun("x", Conditional(Binop(Equals, Var "x", Num 0), Num 1, Binop(Times, Var "x", App(Var "f", 
                     Binop(Minus, Var "x", Num 1))))), App(Var "f", Num 4))) env = Env.Val (Num 24)) "eval_l letrec";
  unit_test (eval_l (Fun ("x", Var "x")) env = Env.Closure (Fun ("x", Var "x"), env)) "eval_l fun";
  unit_test (eval_l (Let ("x", Num 3, Let ("y", Var "x", Let ("x", Num 2, Binop (Plus, Var "y", Num 3))))) env = Env.Val (Num 6)) "eval_l let twice";
  unit_test (eval_l (Let ("x", Num 2, Let ("f", Fun ("y", Binop (Plus, Var "x", Var "y")), 
                     Let ("x", Num 8, App (Var "f", Var "x"))))) env = Env.Val (Num 10)) "eval_l let diff";
  unit_test (eval_l (Fun ("x", Binop (Plus, Var "x", Num 2))) env1 
                     = Env.Closure (Fun ("x", Binop (Plus, Var "x", Num 2)), env1)) "eval_l fun extended env" ;;

let test_all () =
  exprtest ();
  close_test ();
  lookup_test ();
  eval_s_test ();
  eval_d_test ();
  eval_l_test () ;;

let _ = test_all () ;;