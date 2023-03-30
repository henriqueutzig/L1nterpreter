include Types;;
include Expressions;;
include Ops;;

let rec toString (exp:expType):string = match exp with
| TyInt -> "TyInt"
| TyBool -> "TyBool"
| TyFunc (e1 , e2) -> Printf.sprintf "%s -> %s" (toString e1) (toString e2) 
| TyPair (e1 , e2) -> Printf.sprintf "(%s, %s)" (toString e1) (toString e2) 
| TyList (e1) -> Printf.sprintf "List(%s)" (toString e1)
| TyNil  (e1) -> Printf.sprintf "TyNil(%s)" (toString e1)
| TyMaybe (e1) -> Printf.sprintf "TyMaybe(%s)" (toString e1)


let  opToString (operation:op):string = match operation with 
| Sum -> Printf.sprintf "+"
| Diff -> Printf.sprintf "!="
| Eq -> Printf.sprintf "=="
| Less -> Printf.sprintf "<"
| Mult -> Printf.sprintf "*"
| Div -> Printf.sprintf "/"
| Leq -> Printf.sprintf "<="
| Greater -> Printf.sprintf ">"
| Geq -> Printf.sprintf ">="
| And -> Printf.sprintf "&&"
| Or -> Printf.sprintf "||"



let rec expToString (exp:exp):string = match exp with
  | Num(x) -> Printf.sprintf "%d" x
  | Bool(x)   -> Printf.sprintf "%b" x
  | Op(x1,op,x2) -> Printf.sprintf "%s %s %s" (expToString x1) (opToString op) (expToString x2)
  | If(x1,x2,x3) -> Printf.sprintf "If (%s) then (%s) else (%s)" (expToString x1) (expToString x2) (expToString x3)  
  | Var(x) -> Printf.sprintf "Var(%s)" x
  | App(x1,x2) -> Printf.sprintf "App %s to %s" (expToString x1) (expToString x2)
  | Fn(ident,exptype,exp) -> Printf.sprintf "fn %s : %s => {%s} " ident (toString exptype) (expToString exp)
  | Let(ident,expType,exp1,exp2) -> Printf.sprintf "let %s:%s=%s in %s" ident (toString expType) (expToString exp1) (expToString exp2)
  | LetRec(ident,expType,ident2,expType2,exp1,exp2) -> Printf.sprintf "let rec %s:%s = fn %s:%s => %s in %s" 
      ident (toString expType) ident2 (toString expType2) (expToString exp1) (expToString exp2)
  | Pair(x1,x2) -> Printf.sprintf "Pair(%s x %s)" (expToString x1) (expToString x2)
  | Fst(x) -> Printf.sprintf "Fst %s" (expToString x)
  | Snd(x) -> Printf.sprintf "Snd %s" (expToString x)
  | Nil(_) -> Printf.sprintf "Nil"
  | Concat(x1,x2) -> Printf.sprintf "[%s :: %s]" (expToString x1) (expToString x2)
  | Hd(x) -> Printf.sprintf "Hd %s" (expToString x)
  | Tl(x) -> Printf.sprintf "Tl %s" (expToString x)
  | MatchList(e1,e2,e3,x,xs) -> Printf.sprintf "match %s with nil => %s | %s::%s => %s" (expToString e1) (expToString e2) x xs (expToString e3)
  | Just(x) -> Printf.sprintf "Just(%s)" (expToString x)
  | Nothing(_) -> Printf.sprintf "Nothing"
  | MatchOption(e1,e2,e3,x) -> Printf.sprintf "match %s with nothing => %s | Just(%s) => %s" (expToString e1) (expToString e2) x  (expToString e3)


  let rec valueToString v =
    match v with
    | Numeric n -> Printf.sprintf "%d" n
    | Boolean b -> Printf.sprintf "%b" b
    | Closure (param, body, env) ->
        let env_str = List.map (fun (id, v) -> Printf.sprintf "%s = %s" id (valueToString v)) env in
        Printf.sprintf "fun %s -> %s [%s]" param (expToString body) (String.concat ", " env_str)
    | RecClosure (fparam, param, body, env) ->
        let env_str = List.map (fun (id, v) -> Printf.sprintf "%s = %s" id (valueToString v)) env in
        Printf.sprintf "recfun %s %s -> %s [%s]" fparam param (expToString body) (String.concat ", " env_str)
    | Pair (v1, v2) -> Printf.sprintf "(%s, %s)" (valueToString v1) (valueToString v2)
    | Nil _ -> "[]"
    | List (v1, v2) -> Printf.sprintf "[%s, %s]" (valueToString v1) (valueToString v2)
    | Nothing _ -> "nothing"
    | Just v -> Printf.sprintf "just %s" (valueToString v)