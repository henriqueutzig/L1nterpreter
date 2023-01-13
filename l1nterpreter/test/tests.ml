include OUnit2;;
include Lib.Ops;;

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


(******************** 
  TODO: add every new test list into suite's list
*************************)
(* Name the test cases and group them together *)
let suite =
   "Tests">:::
   [sum_tests; 
    diff_tests;
   ];;
  
let () =
  run_test_tt_main suite
  (* run_test_tt_main  *)
;;