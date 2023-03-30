include Lib;;
include Execute;;
include Lib.ToString;;

print_endline "";;
print_endline (expToString((Fst (Concat(Num 2, Nil TyInt)))));;
print_endline (expToString(Op(Num(7), Greater, Num(49))));;
(* print_endline (expToString());; *)

