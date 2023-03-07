include OUnit2
include Lib.Ops
include Lib.TypeInfer
include Lib.ToString
include Lib.Eval

(* include Lib.Expressions;; *)

(*======================= Ops Tests =======================*)
(* SUM *)
let test_sum name in1 in2 out =
  name >:: fun _ -> assert_equal (Numeric out) (sum (Numeric in1) (Numeric in2))

let sum_tests =
  "Sum tests"
  >::: [
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
  name >:: fun _ ->
  assert_equal (Numeric out) (diff (Numeric in1) (Numeric in2))

let diff_tests =
  "Diff tests"
  >::: [
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
  name >:: fun _ ->
  assert_equal (Numeric out) (mult (Numeric in1) (Numeric in2))

let mult_tests =
  "Mult tests"
  >::: [
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
let test_typeInfer name env exp (out : expType) =
  let result = typeInfer env exp in
  let title =
    Printf.sprintf "%s: Assert %s is equal to %s" name (toString out)
      (toString result)
  in
  title >:: fun _ -> assert_equal out (typeInfer env exp)

let typeInfer_tests =
  "typeInfer tests"
  >::: [
         test_typeInfer "if(true, 10, 20)" []
           (If (Bool true, Num 10, Num 20))
           TyInt;
         test_typeInfer "Var(x:Int)" (* nome do teste *)
           [ ("x", TyInt) ] (* ambiente de test *)
           (Var "x") (* expressão a ser testada*)
           TyInt;
         (* tipo esperado*)
         test_typeInfer
           "fn x:Int = 10+x is of TyFunc(TyInt,TyInt)" (* nome do teste *)
           [] (* ambiente de test *)
           (Fn ("x", TyInt, Op (Num 10, Sum, Var "x")))
           (* expressão a ser testada*)
           (TyFunc (TyInt, TyInt));
         (* tipo esperado*)
         test_typeInfer "Let x:Int = 10 in x" (* nome do teste *)
           [] (* ambiente de test *)
           (Let ("x", TyInt, Num 10, Var "x")) (* expressão a ser testada*)
           TyInt;
         (* tipo esperado*)
         test_typeInfer "f(x,y) = x+y"
           [ ("x", TyInt); ("y", TyInt) ]
           (Fn ("x", TyInt, Fn ("y", TyInt, Op (Num 3, Sum, Num 4))))
           (TyFunc (TyInt, TyFunc (TyInt, TyInt)));
         test_typeInfer "Pair(10,Pair(true,20))" (* nome do teste *)
           [] (* ambiente de test *)
           (Pair (Num 10, Pair (Bool true, Num 20)))
           (* expressão a ser testada*)
           (TyPair (TyInt, TyPair (TyBool, TyInt)));
         (* tipo esperado*)
         test_typeInfer "Fst(Pair(10,Pair(true,20)))" (* nome do teste *)
           [] (* ambiente de test *)
           (Fst (Pair (Num 10, Pair (Bool true, Num 20))))
           (* expressão a ser testada*)
           TyInt;
         (* tipo esperado*)
         test_typeInfer "Snd(Pair(10,Pair(true,20)))" (* nome do teste *)
           [] (* ambiente de test *)
           (Snd (Pair (Num 10, Pair (Bool true, Num 20))))
           (* expressão a ser testada*)
           (TyPair (TyBool, TyInt));
         (* tipo esperado*)
         test_typeInfer "Nil" [] (Nil TyInt) (TyList TyInt);
         test_typeInfer "Num(10) is TyInt" [] (Num 10) TyInt;
         test_typeInfer "Concat (Num(10),Nil TyInt) is TyList(TyInt)" []
           (Concat (Num 10, Nil TyInt))
           (TyList TyInt);
         test_typeInfer
           "Concat(Num(20),Concat(Num(10),Nil TyInt)) is TyList(TyInt)" []
           (Concat (Num 20, Concat (Num 10, Nil TyInt)))
           (TyList TyInt);
         test_typeInfer "Head Concat(Num(10),Nil TyInt) is TyInt" []
           (Hd (Concat (Num 10, Nil TyInt)))
           TyInt;
         test_typeInfer "Tail Concat(Num(10),Nil TyInt) is TyList(TyInt)" []
           (Tl (Concat (Num 10, Nil TyInt)))
           (TyList TyInt);
         test_typeInfer
           "Match Concat(Num(10),Nil TyInt) with nil -> True / x::xs -> False \
            is TyBool"
           []
           (MatchList
              (Concat (Num 10, Nil TyInt), Bool true, Bool false, "x", "xs"))
           TyBool;
         test_typeInfer "Match Nothing of TyInt is TyMaybe(TyInt)" []
           (Nothing TyInt) (TyMaybe TyInt);
         test_typeInfer "Match Just(True) is TyMaybe(TyBool)" []
           (Just (Bool false)) (TyMaybe TyBool);
         test_typeInfer
           "Match Just(Num(10)) with Nothing -> True / just x -> False is \
            TyBool"
           []
           (MatchOption (Just (Num 10), Bool true, Bool false, "x"))
           TyBool;
         test_typeInfer "Diff is Ok"
           [ ("x", TyInt) ]
           (Op (Var "x", Diff, Num 1))
           TyInt;
         test_typeInfer "App is Ok"
           [ ("fat", TyFunc (TyInt, TyInt)); ("x", TyInt) ]
           (App (Var "fat", Op (Var "x", Diff, Num 1)))
           TyInt;
         test_typeInfer "Function is correctly stored"
           [ ("fat", TyFunc (TyInt, TyInt)) ]
           (Var "fat")
           (TyFunc (TyInt, TyInt));
         test_typeInfer "Fatorial body is Well Typed"
            [ ("fat", TyFunc (TyInt, TyInt)); ("x", TyInt) ]
            (If
               ( Op (Var "x", Eq, Num 0),
                 Num 1,
                 Op (Var "x", Mult, App (Var "fat", Op (Var "x", Diff, Num 1)))
               ))
            TyInt;
         test_typeInfer
            "Let Rec fat:TyInt->TyBool= fn x:TyInt => if x==0 then 1 else x * \
             fat(x-1_ in fat 5"
            []
            (LetRec
               ( "fat",
                 TyFunc (TyInt, TyInt),
                 "x",
                 TyInt,
                 If
                   ( Op (Var "x", Eq, Num 0),
                     Num 1,
                     Op
                       (Var "x", Mult, App (Var "fat", Op (Var "x", Diff, Num 1)))
                   ),
                 App (Var "fat", Num 5) ))
            TyInt
         (*test_typeInfer "" (*nome do teste *)
                       [] (* ambiente de test *)
                       () (* expressão a ser testada*)
                       ();(* tipo esperado*) *)
       ]


(* ********EVAL******** *)
let test_eval name env exp (out : value) =
  let result = eval env exp in
  let title = name
    (* Printf.sprintf "%s: Assert %s is equal to %s" name (toString out)
      (toString result) *)
  in
  title >:: fun _ -> assert_equal out result

let eval_tests =
  "eval tests"
  >::: [
         test_eval "Num(10)" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Num(10)) (* expressão a ser testada*)
                       (Numeric(10));(* tipo esperado*) 
          test_eval "Num(-102)" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Num(-102)) (* expressão a ser testada*)
                       (Numeric(-102));(* tipo esperado*) 
          test_eval "Bool(true)" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Bool(true)) (* expressão a ser testada*)
                       (Boolean(true));(* tipo esperado*) 
          test_eval "Bool(false))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Bool(false)) (* expressão a ser testada*)
                       (Boolean(false));(* tipo esperado*)
          test_eval "Var(x) = 10" (*nome do teste *)
                       [("x", Numeric(10))] (* ambiente de test *)
                       (Var("x")) (* expressão a ser testada*)
                       (Numeric(10));(* tipo esperado*)
          test_eval "Var(x) = false" (*nome do teste *)
                       [("x", Boolean(false))] (* ambiente de test *)
                       (Var("x")) (* expressão a ser testada*)
                       (Boolean(false));(* tipo esperado*)  
          test_eval "(Numeric(10) Sum Numeric(-2))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(10), Sum, Num(-2))) (* expressão a ser testada*)
                       (Numeric(8));(* tipo esperado*) 
          test_eval "(Numeric(5) Diff Numeric(3))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(5), Diff, Num(3))) (* expressão a ser testada*)
                       (Numeric(2));(* tipo esperado*) 
          test_eval "(Numeric(2) Mult Numeric(8))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(2), Mult, Num(8))) (* expressão a ser testada*)
                       (Numeric(16));(* tipo esperado*) 
          test_eval "(Numeric(50) Div Numeric(5))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(50), Div, Num(5))) (* expressão a ser testada*)
                       (Numeric(10));(* tipo esperado*) 
          test_eval "(Numeric(7) Eq Numeric(49))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(7), Eq, Num(49))) (* expressão a ser testada*)
                       (Boolean(false));(* tipo esperado*) 
          test_eval "(Numeric(7) Eq Numeric(7))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(7), Eq, Num(7))) (* expressão a ser testada*)
                       (Boolean(true));(* tipo esperado*) 
          test_eval "(Numeric(7) Less Numeric(49))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(7), Less, Num(49))) (* expressão a ser testada*)
                       (Boolean(true));(* tipo esperado*) 
          test_eval "(Numeric(7) Less Numeric(6))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(7), Less, Num(6))) (* expressão a ser testada*)
                       (Boolean(false));(* tipo esperado*) 
          test_eval "(Numeric(7) Leq Numeric(7))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(7), Leq, Num(7))) (* expressão a ser testada*)
                       (Boolean(true));(* tipo esperado*) 
          test_eval "(Numeric(7) Leq Numeric(6))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(7), Leq, Num(6))) (* expressão a ser testada*)
                       (Boolean(false));(* tipo esperado*) 
          test_eval "(Numeric(49) Greater Numeric(7))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(49), Greater, Num(7))) (* expressão a ser testada*)
                       (Boolean(true));(* tipo esperado*) 
          test_eval "(Numeric(7) Greater Numeric(49))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(7), Greater, Num(49))) (* expressão a ser testada*)
                       (Boolean(false));(* tipo esperado*) 
          test_eval "(Numeric(7) Geq Numeric(7))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(7), Geq, Num(7))) (* expressão a ser testada*)
                       (Boolean(true));(* tipo esperado*) 
          test_eval "(Numeric(7) Geq Numeric(8))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Num(7), Geq, Num(8))) (* expressão a ser testada*)
                       (Boolean(false));(* tipo esperado*) 
          test_eval "(Boolean(true) opAnd Boolean(true))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Bool(true), And, Bool(true))) (* expressão a ser testada*)
                       (Boolean(true));(* tipo esperado*) 
          test_eval "(Boolean(true) opAnd Boolean(false))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Bool(true), And, Bool(false))) (* expressão a ser testada*)
                       (Boolean(false));(* tipo esperado*)
          test_eval "(Boolean(false) opOr Boolean(false))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Bool(false), And, Bool(false))) (* expressão a ser testada*)
                       (Boolean(false));(* tipo esperado*) 
          test_eval "(Boolean(false) opAnd Boolean(true))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (Op(Bool(false), Or, Bool(true))) (* expressão a ser testada*)
                       (Boolean(true));(* tipo esperado*)
          test_eval "(If Boolean(true) then Num(1) else Num(0))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (If(Bool(true), Num(1), Num(0))) (* expressão a ser testada*)
                       (Numeric(1));(* tipo esperado*)
          test_eval "(If Boolean(false) then Num(1) else Num(0))" (*nome do teste *)
                       [] (* ambiente de test *)
                       (If(Bool(false), Num(1), Num(0))) (* expressão a ser testada*)
                       (Numeric(0));(* tipo esperado*)
       ]

(********************
    TODO: add every new test list into suite's list
  *************************)
(* Name the test cases and group them together *)
let suite = "Tests" >::: [ sum_tests; diff_tests; mult_tests; typeInfer_tests; eval_tests ]
let () = run_test_tt_main suite
(* run_test_tt_main  *)
