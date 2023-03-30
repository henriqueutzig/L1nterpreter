include Lib;;
include Execute;;
include Lib.ToString;;

print_endline "";;
print_endline (expToString((Fst (Concat(Num 2, Nil TyInt)))));;
print_endline (expToString(Op(Num(7), Greater, Num(49))));;
print_endline "";
(* print_endline (expToString());; *)
run (Op(Num(7), Greater, Num(49)));;
run ( (
  LetRec(
  "lookup",
  TyFunc(TyList(TyPair(TyInt,TyInt)),TyFunc(TyInt, TyMaybe(TyInt))),
 "l",
  TyList(TyPair(TyInt,TyInt)),
  Fn(
    "key",
    TyInt,
    MatchList(
      Var "l",
      Nothing(TyInt),
      If(
        Op(Fst(Var "x"),Eq,Var "key"),
        Just(Snd(Var "x")),
        App(App(Var "lookup", Var ("xs")), Var "key")
      ),
      "x",
      "xs"
  )),
  App(App(Var "lookup",Concat(
    Pair(Num 1, Num 10), 
    Concat(
      Pair(Num 2, Num 20), 
      Concat(
        Pair(Num 3, Num 30),Nil(TyPair(TyInt,TyInt))
        )
        )
  )
    ), Num 2 ))));;


(* let x:int = 2 in
  let foo: int --> int = fn y:int => x + y in
   let x: int = 5
   in foo 10 *)
    run(
      Let("x",TyInt,Num(2),
        Let("foo",TyFunc(TyInt,TyInt),
          Fn("y",TyInt,Op(Var("x"),Sum,Var("y"))),
          Let("x",TyInt,Num(5),App(Var("foo"),Num(10)))
    )));;

    run(Let("x",TyInt,Num(2),
      Let("foo",TyFunc(TyInt,TyInt),
        Fn("y",TyInt,Op(Var("x"),Sum,Var("y"))),
        Let("x",TyInt,Num(5),Var("foo"))
    )));;


    run(LetRec(
      "map",
      TyFunc(TyFunc(TyInt,TyInt),TyFunc(TyList(TyInt),TyList(TyInt))),
      "f",
      TyFunc(TyInt,TyInt),
      Fn(
              "l",
              TyList(TyInt),
              MatchList(
                Var("l"),
                Nil(TyInt),
                Concat(
                  App(Var("f"),Var("x")),
                  App(
                    App(Var("map"),Var("f")),
                    Var("xs"))),
                "x",
                "xs")
              ),
        App(App(Var("map"),Fn("x",TyInt,Op(Var("x"),Sum,Var("x")))),Concat(Num(10),Concat(Num(20),Concat(Num(30),Nil(TyInt)))))
        )
    );;

run(LetRec(
  "pow",
  TyFunc(TyInt,TyFunc(TyInt,TyInt)),
  "x",
  TyInt,
  Fn(
    "y",
    TyInt,
    If(
      (Op(Var("y"),Eq,Num(0))),
      Num(1),
      Op(Var("x"),Mult,App(
        App(Var("pow"),Var("x")),
        Op(Var("y"),Diff,Num(1))
        ))
    )),
    App(App(Var("pow"),Num(3)),Num(4))
  )
);;