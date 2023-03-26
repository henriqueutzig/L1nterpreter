include Eval;;
let run (expression:exp) : value  =
  match typeInfer [] expression with
   | _ -> eval [] expression   
  