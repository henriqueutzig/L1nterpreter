include Eval;;
include ToString;;
let run (expression:exp) : value  =
  match typeInfer [] expression with
   | _ -> eval [] expression   
  