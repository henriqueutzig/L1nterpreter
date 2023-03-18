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

exception IncorrectExpresionType;;
exception ApplicationWNoFunc;;

let rec eval (env: valEnv) (e: exp) : value = match e with
  (* Value rules *)
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
  (* If rule *)
  | If (e1,e2,e3) -> 
    (match (eval env e1) with
        | Boolean(true) -> (eval env e2)
        | Boolean(false) -> (eval env e3)
        | _ -> raise IncorrectExpresionType)
  (* Just rule *)
  | Just e -> Just (eval env e)
  (* Nothing rule *)
  | Nothing t -> Nothing t
  (* Let  rule *)
  | Let (x, _, e1, e2) -> 
    (let env' : valEnv = (updateValEnv env x (eval env e1)) in
      (eval env' e2))
  (* Fn rule *)
  | Fn (x, _, e') -> (Closure(x, e', env))
  (* APP *)
  (* | App(e1, e2) -> 
    (match ((eval env e1), (eval env e2)) with
      | (Closure(x, e, env), v) -> (eval (updateValEnv env x v) e)
      | (RecClosure(f, x, e, env), v) -> (eval (updateValEnv env x v) e)
      | _ -> raise ApplicationWNoFunc
    ) *)
  | _ -> failwith "pattern matching not exaustive"