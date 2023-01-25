include Types;;
include Expressions;;

exception IncorretExpType;;

let rec typeInfer (env: tyEnv) (e: exp) : expType =  match e with
  (* Basic expressions *)
  | Num  _ -> TyInt
  | Bool _ -> TyBool
  (* Op expressions *)
  | Op (x1,Sum,x2) -> 
     (match (typeInfer env x1, typeInfer env x2) with
      (TyInt, TyInt) -> TyInt
      | _ -> raise IncorretExpType)
  | Op (x1,Diff,x2) -> 
    (match (typeInfer env x1, typeInfer env x2) with
      (TyInt, TyInt) -> TyInt
      | _ -> raise IncorretExpType)
  | Op (x1,Mult,x2) -> 
    (match (typeInfer env x1, typeInfer env x2) with
      (TyInt, TyInt) -> TyInt
      | _ -> raise IncorretExpType)
  | Op (x1,Div,x2) -> 
    (match (typeInfer env x1, typeInfer env x2) with
      (TyInt, TyInt) -> TyInt
      | _ -> raise IncorretExpType)
  | Op (x1,Eq,x2) -> 
    (match (typeInfer env x1, typeInfer env x2) with
      (TyInt, TyInt) -> TyBool
      | _ -> raise IncorretExpType)
  | Op (x1,Less,x2) -> 
    (match (typeInfer env x1, typeInfer env x2) with
      (TyInt, TyInt) -> TyBool
      | _ -> raise IncorretExpType)
  | Op (x1,Leq,x2) -> 
    (match (typeInfer env x1, typeInfer env x2) with
      (TyInt, TyInt) -> TyBool
      | _ -> raise IncorretExpType)
  | Op (x1,Greater,x2) -> 
    (match (typeInfer env x1, typeInfer env x2) with
      (TyInt, TyInt) -> TyBool
      | _ -> raise IncorretExpType)
  | Op (x1,Geq,x2) -> 
    (match (typeInfer env x1, typeInfer env x2) with
      (TyInt, TyInt) -> TyBool
      | _ -> raise IncorretExpType)
  | Op (x1,And,x2) -> 
    (match (typeInfer env x1, typeInfer env x2) with
      (TyBool, TyBool) -> TyBool
      | _ -> raise IncorretExpType)
  | Op (x1,Or,x2) -> 
    (match (typeInfer env x1, typeInfer env x2) with
      (TyBool, TyBool) -> TyBool
      | _ -> raise IncorretExpType)
  (* IF expression *)
  | If (e1, e2, e3) ->
    (match (typeInfer env e1, typeInfer env e2, typeInfer env e3) with
      (TyBool, t2, t3) when t2 == t3 -> t2
      | _ -> raise IncorretExpType)
  | Var(x) -> 
    (match (lookUpEnv env x) with
      | TyInt  -> TyInt
      | TyBool -> TyBool
      | _ -> raise IncorretExpType
    )
  (* ========== TODO: Ap, Let, Fn, LetRec ============== *)
  (* | Ap(e1, e2) ->
    (match (typeInfer e1, typeInfer e2) with
      | 
    ) *)
  (*| Fn() *)