open Lib


type expr =
 | Num of int
 | Op of expr * Ops.op * expr
let numero1 = Num 1;;
let numero2 = Num 2;;
let _ = Op(numero1, Ops.Sum,  numero2);;

let tipo = TypeInfer.typeInfer [] (Nil TyInt);;

print_string "\n + ^ (ToString.toString tipo) ^";;

