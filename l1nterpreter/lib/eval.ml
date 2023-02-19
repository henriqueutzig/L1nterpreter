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
  | _ -> failwith "pattern matching not exaustive"