include Ops;;
include Types;;

(* type varible *)
type ident = string

(* ****************************** AMBIENT/ENV ****************************** *)
(* ambient/env type *)
type tyEnv = (ident * expType) list

(* Auxiliary ambient/env functions *)
let updateEnv (env: tyEnv) (id: ident) (t: expType) : tyEnv =
  (id, t) :: env

(* Varible not declared exception *)
exception VaribleNotDeclared;; 
let rec lookUpEnv (env: tyEnv) (id: ident) : expType = match env with
| (id', t') :: n -> if id' = id then t' else (lookUpEnv n id) 
| [] -> raise VaribleNotDeclared
(* ****************************** AMBIENT/ENV ****************************** *)


(* type expression *)
type exp = 
  | Num    of int
  | Bool   of bool
  | Op     of exp * op * exp
  | If     of exp * exp * exp
  | Var    of ident  (* x *)
  (* application *)
  | App    of exp * exp
  (* functions *)
  | Fn     of ident * exp
  | Let    of ident * exp * exp
  | LetRec of ident * ident * exp * exp
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
  | Closure of ident * exp * tyEnv
  | RecClosure of ident * ident * exp * tyEnv

(* binary ops  *)
exception IncorretValueType;;

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