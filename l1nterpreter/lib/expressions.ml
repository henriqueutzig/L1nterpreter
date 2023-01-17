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