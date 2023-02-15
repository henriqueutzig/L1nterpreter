include OUnit2;;
include Lib.Ops;;
include Lib.TypeInfer;;
include Lib.ToString;;
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
let test_typeInfer name env exp (out:expType) = 
  let result = typeInfer env exp in
  let title = Printf.sprintf "%s: Assert %s is equal to %s" name (toString out) (toString result) in
  title >:: (fun _ -> assert_equal 
  out (typeInfer env exp))

  

  let typeInfer_tests = "typeInfer tests" >::: [
  test_typeInfer "if(true, 10, 20)" 
                [] 
                (If(Bool(true), Num(10), Num(20)))
                (TyInt);
  test_typeInfer "Var(x:Int)" (* nome do teste *)
                [("x", TyInt)] (* ambiente de test *)
                (Var("x")) (* expressão a ser testada*)
                (TyInt); (* tipo esperado*)
  test_typeInfer "fn x:Int = 10+x is of TyFunc(TyInt,TyInt)" (* nome do teste *)
                [] (* ambiente de test *)
                (Fn("x", TyInt, Op(Num(10),Sum,Var("x")))) (* expressão a ser testada*)
                (TyFunc(TyInt,TyInt)); (* tipo esperado*)  
  test_typeInfer "Let x:Int = 10 in x" (* nome do teste *)
                [] (* ambiente de test *)
                (Let("x", TyInt, Num(10), Var("x"))) (* expressão a ser testada*)
                (TyInt); (* tipo esperado*)              
  (* test_typeInfer "f(x,y) = x+y" 
                [("x", TyInt);("y", TyInt)] 
                (Fn("x", Fn("y", Op(Num(3), Sum, Num(4)))))
                (TyFunc (TyInt, TyFunc(TyInt, TyInt))); *)
  test_typeInfer "Pair(10,Pair(true,20))" (* nome do teste *)
                [] (* ambiente de test *)
                (Pair(Num(10), Pair(Bool(true), Num(20)))) (* expressão a ser testada*)
                (TyPair(TyInt, TyPair(TyBool, TyInt))); (* tipo esperado*)
  test_typeInfer "Fst(Pair(10,Pair(true,20)))" (* nome do teste *)
                [] (* ambiente de test *)
                (Fst(Pair(Num(10), Pair(Bool(true), Num(20))))) (* expressão a ser testada*)
                (TyInt); (* tipo esperado*)
  test_typeInfer "Snd(Pair(10,Pair(true,20)))" (* nome do teste *)
                [] (* ambiente de test *)
                (Snd(Pair(Num(10), Pair(Bool(true), Num(20))))) (* expressão a ser testada*)
                (TyPair(TyBool, TyInt)); (* tipo esperado*)
  test_typeInfer "Nil" 
              []
              (Nil TyInt)
              (TyList(TyInt));
  test_typeInfer "Num(10) is TyInt" 
              []
              (Num(10))
              (TyInt);
  test_typeInfer "Concat (Num(10),Nil TyInt) is TyList(TyInt)"
              []
              (Concat(Num(10),(Nil TyInt)))
              (TyList(TyInt));
  test_typeInfer "Concat(Num(20),Concat(Num(10),Nil TyInt)) is TyList(TyInt)"
              []
              (Concat(Num(20),Concat(Num(10),(Nil TyInt))))
              (TyList(TyInt));
  test_typeInfer "Head Concat(Num(10),Nil TyInt) is TyInt"
              []
              (Hd(Concat(Num(10),(Nil TyInt))))
              (TyInt);
  test_typeInfer "Tail Concat(Num(10),Nil TyInt) is TyList(TyInt)"
              []
              (Tl(Concat(Num(10),(Nil TyInt))))
              (TyList(TyInt));
  test_typeInfer "Match Concat(Num(10),Nil TyInt) with nil -> True / x::xs -> False is TyBool"
              []
              (MatchList(Concat(Num(10),(Nil TyInt)), Bool(true), Bool(false),"x","xs"))
              (TyBool);
  test_typeInfer "Match Nothing of TyInt is TyMaybe(TyInt)"
              []
              (Nothing(TyInt))
              (TyMaybe(TyInt));
  test_typeInfer "Match Just(True) is TyMaybe(TyBool)"
              []
              (Just(Bool(false)))
              (TyMaybe(TyBool));
  test_typeInfer "Match Just(Num(10)) with Nothing -> True / just x -> False is TyBool"
              []
              (MatchOption(Just(Num(10)), Bool(true), Bool(false),"x"))
              (TyBool);
  (*test_typeInfer "" (*nome do teste *)
                [] (* ambiente de test *)
                () (* expressão a ser testada*)
                ();(* tipo esperado*) *)
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