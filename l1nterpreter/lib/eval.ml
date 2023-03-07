include TypeInfer;;

(* ****************************** AMBIENT/ENV ****************************** *)
(* ambient/env type *)
type valEnv = (ident * value) list

(* Auxiliary ambient/env functions *)
let updateValEnv (env: valEnv) (id: ident) (v: value) : valEnv =
  (id, v) :: env

(* Varible not declared exception *)
exception ValueNotAttributed;; 
let rec lookUpValEnv (env: valEnv) (id: ident) : value = match env with
| (id', v') :: n -> if id' = id then v' else (lookUpValEnv n id) 
| [] -> raise ValueNotAttributed
(* ****************************** AMBIENT/ENV ****************************** *)

let rec eval (env: valEnv) (e: exp) : value = match e with
  | Num v -> Numeric(v)
  | Bool v -> Boolean(v)
  | Var(x) -> (lookUpValEnv env x)
  (* Op expressions *)
  | Op (x1,Sum,x2) -> (sum (eval env x1) (eval env x2))
  | Op (x1,Diff,x2) -> (diff (eval env x1) (eval env x2))
  | Op (x1,Mult,x2) -> (mult (eval env x1) (eval env x2))
  | Op (x1,Div,x2) -> (div (eval env x1) (eval env x2))
  | Op (x1,Eq,x2) -> (eq (eval env x1) (eval env x2))
  | Op (x1,Less,x2) -> (less (eval env x1) (eval env x2))
  | Op (x1,Leq,x2) -> (leq (eval env x1) (eval env x2))
  | Op (x1,Greater,x2) -> (greater (eval env x1) (eval env x2))
  | Op (x1,Geq,x2) -> (geq (eval env x1) (eval env x2))
  | Op (x1,And,x2) -> (opAnd (eval env x1) (eval env x2))
  | Op (x1,Or,x2) -> (opOr (eval env x1) (eval env x2))
  (* If rules *)
  | If (e1,e2,e3) -> (match (eval env e1) with
                        Boolean(true) -> (eval env e2)
                        | Boolean(false) -> (eval env e3))
  | _ -> failwith "pattern matching not exaustive"