include OUnit2;;
include Lib.Ops;;

(*======================= Ops Tests =======================*)
(* let test_sum _ =
  assert_equal (Numeric 2) (sum (Numeric 1) (Numeric 1)) ;; *)
let test_sum name in1 in2 out =
  name >:: (fun _ -> assert_equal (Numeric out) (sum (Numeric in1)  (Numeric in2)))

let sum_tests = "test suite for sum" >::: [
  test_sum "1+1" 1 1 2;
  test_sum "0+0" 0 0 0;
  test_sum "1+0)" 1 0 1;
  test_sum "0+1" 0 1 1;
  test_sum "-1+1" (-1) 1 0;
  test_sum "-1+(-1)" (-1) (-1) (-2);
]


(******************** 
  TODO: add every new test list into suite's list
*************************)
(* Name the test cases and group them together *)
let suite =
   "Tests">:::
   [sum_tests; 
   ];;
  
let () =
  run_test_tt_main suite
  (* run_test_tt_main  *)
;;