include Types
let rec toString (exp:expType):string = match exp with
| TyInt -> "TyInt"
| TyBool -> "TyBool"
| TyFunc (e1 , e2) -> Printf.sprintf "%s -> %s" (toString e1) (toString e2) 
| TyPair (e1 , e2) -> Printf.sprintf "(%s, %s)" (toString e1) (toString e2) 
| TyList (e1) -> Printf.sprintf "List(%s)" (toString e1)
| TyNil  (e1) -> Printf.sprintf "TyNil(%s)" (toString e1)
| TyMaybe (e1) -> Printf.sprintf "TyMaybe(%s)" (toString e1)