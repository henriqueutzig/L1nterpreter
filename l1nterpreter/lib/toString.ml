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


let rec opToString (operation:op):string = match operation with 
| _ -> Printf.sprintf "Not implemented yet"


let rec expToString (exp:exp):string = match exp with
  | Num(x) -> Printf.sprintf "%d" x
  | Bool(x)   -> Printf.sprintf "%b" x
  | Op(x1,op,x2) -> Printf.sprintf "%s %s %s" (expToString x1) (opToString op) (expToString x2)
  | _ -> Printf.sprintf "Not implemented"
  (* | If     of exp * exp * exp
  | Var    of ident  (* x *)
  (* application *)
  | App    of exp * exp
  (* functions *)
  | Fn     of ident * expType * exp
  | Let    of ident * expType * exp * exp
  | LetRec of ident * expType * ident * expType * exp * exp
  (* pair *)
  | Pair   of exp * exp
  | Fst    of exp
  | Snd    of exp
  (* list *)
  | Nil    of expType
  | Concat of exp * exp  (* e1 :: e2 *)
  | Hd     of exp
  | Tl     of exp
  | MatchList of exp * exp * exp * ident * ident (* TODO:  match e1 with nil ⇒ e2 | x::xs ⇒ e3 *)
  (* option type *)
  | Just   of exp
  | Nothing of expType
  | MatchOption of exp * exp * exp * ident TODO:  match e1 with nothing ⇒ e2 | just x ⇒ e3 *)
