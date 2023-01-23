include Ops;;

(* type varible *)
type var = string

(* type expression *)
type exp = 
  | Num    of int
  | Bool   of bool
  | Op     of exp * op * exp
  | If     of exp * exp * exp
  | Var    of var  (* x *)
  (* application *)
  | App    of exp * exp
  (* functions *)
  | Fn     of var * exp
  | Let    of var * exp * exp
  | LetRec of var * var * exp * exp
  (* pair *)
  | Pair   of exp * exp
  | Fst    of exp
  | Snd    of exp
  (* list *)
  | Nil 
  | Concat of exp * exp  (* e1 :: e2 *)
  | Hd     of exp
  | Tl     of exp
  (* matchs *)
  | Just   of exp
  | Nothing 
  (* Match with Nil *)
  | MatchNil of exp * exp  (* TODO:  match e1 with nil ⇒ e2 | x::xs ⇒ e3 *)
  (* Match with nothing *)
  | MatchNothing of exp * exp (* TODO:  match e1 with nothing ⇒ e2 | just x ⇒ e3*)


type value = 
  | Numeric of int
  | Boolean of bool
  | Closure of var * exp * ambient
  | RecClosure of var * var * exp * ambient
  and   ambient = (var * value) list

(* Varible not declared exception *)
exception VaribleNotDeclared;;

(* Auxiliary ambient functions *)
let updateAmbient (amb: ambient) (x :var) (v :value) : ambient =
  (x, v) :: amb

let rec lookUpAmbient (amb: ambient) (x :var) : value = match amb with
  | [] -> raise VaribleNotDeclared
  | (var, value) :: t -> if var = x then value else (lookUpAmbient t x) 

exception IncorretValueType

let sum (x: value)(y: value) : value = match (x, y) with
  | (Numeric(x), Numeric(y)) -> Numeric(x+y)
  | _ -> raise IncorretValueType

let diff (x: value)(y: value) : value = match (x, y) with
  | (Numeric(x), Numeric(y)) -> Numeric(x-y)
  | _ -> raise IncorretValueType

let mult (x: value)(y: value) : value = match (x, y) with
  | (Numeric(x), Numeric(y)) -> Numeric(x*y)
  | _ -> raise IncorretValueType

let div (x: value)(y: value) : value = match (x, y) with
  | (Numeric(x), Numeric(y)) -> Numeric(x / y)
  | _ -> raise IncorretValueType

let eq (x: value)(y: value) : value = match (x, y) with
  | (Numeric(x), Numeric(y)) -> Boolean(x = y)
  | _ -> raise IncorretValueType  

let less (x: value)(y: value) : value = match (x, y) with
  | (Numeric(x), Numeric(y)) -> Boolean(x < y)
  | _ -> raise IncorretValueType

let leq (x: value)(y: value) : value = match (x, y) with
  | (Numeric(x), Numeric(y)) -> Boolean(x <= y)
  | _ -> raise IncorretValueType

let greater (x: value)(y: value) : value = match (x, y) with
  | (Numeric(x), Numeric(y)) -> Boolean(x > y)
  | _ -> raise IncorretValueType

let geq (x: value)(y: value) : value = match (x, y) with
  | (Numeric(x), Numeric(y)) -> Boolean(x >= y)
  | _ -> raise IncorretValueType  

let opAnd (x: value)(y: value) : value = match (x, y) with
  | (Boolean(x), Boolean(y)) -> Boolean(x && y)
  | _ -> raise IncorretValueType
  
let opOr (x: value)(y: value) : value = match (x, y) with
  | (Boolean(x), Boolean(y)) -> Boolean(x || y)
  | _ -> raise IncorretValueType  