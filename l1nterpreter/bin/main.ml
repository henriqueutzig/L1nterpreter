include Lib;;
include Execute;;
include Lib.ToString;;

print_endline "";;
print_endline (expToString((Fst (Concat(Num 2, Nil TyInt)))));;
