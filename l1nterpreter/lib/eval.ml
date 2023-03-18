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
exception GetElementOfEmptyList;;

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
  (* Pattern Matching - Maybe T expressions *)
  | MatchOption(e1, e2, e3, x) -> 
    (match (eval env e1) with
      | Nothing(_) -> (eval env e2)
      | Just(x') -> (eval (updateValEnv env x x') e3)
      | _ -> raise IncorrectExpresionType
    )
  (* Let  rule *)
  | Let (x, _, e1, e2) -> 
    (let env' : valEnv = (updateValEnv env x (eval env e1)) in
      (eval env' e2))
  (* Fn rule *)
  | Fn (x, _, e') -> (Closure(x, e', env))
  (* LetRec rule *)
  | LetRec(f, _, x, _, e1, e2) -> (eval (updateValEnv env f (RecClosure(f, x, e1, env))) e2)
  (* APP *)
  | App(e1, e2) -> 
    (match ((eval env e1), (eval env e2)) with
      | (Closure(x, e, env), v) -> (eval (updateValEnv env x v) e)
      | (RecClosure(f, x, e, env), v) -> (eval (updateValEnv (updateValEnv env x v) f (RecClosure(f, x, e, env))) e)
      | _ -> raise ApplicationWNoFunc
    )
  (* Pairs rules *)
  | Pair(e1, e2) -> Pair((eval env e1), (eval env e2))
  | Fst(e) -> 
    (match (eval env e) with
      | Pair(v1, _) -> v1
      | _ -> raise IncorrectExpresionType
    )
  | Snd(e) ->
    (match (eval env e) with
      | Pair(_, v2) -> v2
      | _ -> raise IncorrectExpresionType
    )
  (* List rules *)
  | Nil(t) -> Nil(t)
  | Concat(e1,e2) -> List((eval env e1), (eval env e2)) (* WARNNING: check tests for possible problem *)
  | Hd(e') -> 
    (match (eval env e') with
      | List(v1, _) -> v1
      | Nil(_) -> raise GetElementOfEmptyList
      | _ -> raise IncorrectExpresionType
    )
  | Tl(e') -> 
      (match (eval env e') with
        | List(_, v2) -> v2
        | Nil(_) -> raise GetElementOfEmptyList
        | _ -> raise IncorrectExpresionType
      )
  | _ -> failwith "pattern matching not exaustive"