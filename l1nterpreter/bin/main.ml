open Lib

type expr =
 | Num of int
 | Op of expr * Ops.operators * expr
let numero1 = Num 1;;
let numero2 = Num 2;;
let _ = Op(numero1, Ops.Sum,  numero2)