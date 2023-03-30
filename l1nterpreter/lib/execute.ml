include Eval;;
include ToString;;

exception EvalError;;
exception TypeInferError;;

let run (expression:exp) : value  =
  let () = print_string "Expression " in
  let () = print_endline (expToString expression) in 
  try 
  let exp_type = typeInfer [] expression in
  let () = print_string "Type of expression is: " in 
  let () = print_endline (toString exp_type)
in 
try 
let result = eval [] expression in
let () = print_string "Result of eval is: " in
let () = print_endline (valueToString result) in result
with 
| _  -> let () = print_endline "Error in eval step" in raise EvalError
with 
| _ -> let () = print_endline "Failed on typeInfer step" in raise TypeInferError


  