include OUnit2;;
include Lib.Ops;;
include Lib.TypeInfer;;
(* include Lib.Expressions;; *)

(*======================= Ops Tests =======================*)
(* SUM *)
let test_sum name in1 in2 out =
  name >:: (fun _ -> assert_equal (Numeric out) (sum (Numeric in1)  (Numeric in2)))

let sum_tests = "Sum tests" >::: [
  test_sum "1+1" 1 1 2;
  test_sum "0+0" 0 0 0;
  test_sum "1+0" 1 0 1;
  test_sum "0+1" 0 1 1;
  test_sum "(-1)+0" (-1) 0 (-1);
  test_sum "0+(-1)" 0 (-1) (-1);
  test_sum "-1+1" (-1) 1 0;
  test_sum "1+(-1)" 1 (-1) 0;
  test_sum "-1+(-1)" (-1) (-1) (-2);
]

(* DIFF *)
let test_diff name in1 in2 out =
  name >:: (fun _ -> assert_equal (Numeric out) (diff (Numeric in1)  (Numeric in2)))

let diff_tests = "Diff tests" >::: [
  test_diff "1-1" 1 1 0;
  test_diff "0-0" 0 0 0;
  test_diff "1-0" 1 0 1;
  test_diff "0-1" 0 1 (-1);
  test_diff "(-1)-0" (-1) 0 (-1);
  test_diff "0-(-1)" 0 (-1) 1;
  test_diff "-1-1" (-1) 1 (-2);
  test_diff "1-(-1)" 1 (-1) 2;
  test_diff "-1-(-1)" (-1) (-1) 0;
]

(* MULT *)
let test_mult name in1 in2 out =
  name >:: (fun _ -> assert_equal (Numeric out) (mult (Numeric in1)  (Numeric in2)))

let mult_tests = "Mult tests" >::: [
  test_mult "1*3" 1 3 3;
  test_mult "0*0" 0 0 0;
  test_mult "1*0" 1 0 0;
  test_mult "0*1" 0 1 0;
  test_mult "(-1)*0" (-1) 0 0;
  test_mult "0*(-1)" 0 (-1) 0;
  test_mult "-2*3" (-2) 3 (-6);
  test_mult "5*(-3)" 5 (-3) (-15);
  test_mult "-4*(-3)" (-4) (-3) 12;
]

(* ********TYPEINFER******** *)
let test_typeInfer name env exp out = 
  name >:: (fun _ -> assert_equal out (typeInfer env exp))

let typeInfer_tests = "typeInfer tests" >::: [
  test_typeInfer "f(x,y) = x+y" 
                [("x", TyInt);("y", TyInt)] 
                (Fn("x", Fn("y", Op(Num(3), Sum, Num(4)))))
                (TyFunc (TyInt, TyInt));
  (* test_typeInfer "" ;
  test_typeInfer "" ;
  test_typeInfer "" ;
  test_typeInfer "" ;
  test_typeInfer "" ;
  test_typeInfer "" ;
  test_typeInfer "" ;
  test_typeInfer "" ; *)
]
(******************** 
  TODO: add every new test list into suite's list
*************************)
(* Name the test cases and group them together *)
let suite =
   "Tests">:::
   [sum_tests; 
    diff_tests;
    mult_tests;
    typeInfer_tests;
   ];;
  
let () =
  run_test_tt_main suite
  (* run_test_tt_main  *)
;;