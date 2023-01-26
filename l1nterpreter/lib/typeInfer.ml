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
  | Let(id, t, e1, e2) -> 
    (match (typeInfer env e1, typeInfer (updateEnv env id t) e2) with
      | (_, t2) -> t2
    )
  | Fn(id, e) ->
    let t1 = lookUpEnv env id in
    let t2 = typeInfer env e in
      TyFunc(t1, t2)
  | LetRec(fid, inputid, e1, e2) -> 
    (
      let ftype = (lookUpEnv env fid) in
        let e2type = typeInfer (updateEnv env fid ftype) e2 in 
          let inputtype = lookUpEnv env inputid in 
            let e1type = typeInfer (updateEnv env inputid inputtype) e1 in 
            match (ftype) with
            | TyFunc(inputtype', e1type') -> 
              if ((inputtype' = inputtype) && (e1type' = e1type)) then e2type else raise IncorretExpType
            | _ -> raise IncorretExpType
      (* match (lookUpType amb id, ty, typeInfer amb e1, typeInfer amb e2)
      | (id', ty, e1', e2') -> e2' *)
    )
    (* ========== TODO: Concat, Hd, Tl, Nil, Nothing,
                  Just, MatchNil MatchNothing ============== *)
  | App(e1, e2) ->
    (let t1 = typeInfer env e1 in
      let t2 = typeInfer env e2 in 
        match t1 with 
         | TyFunc(t1', t2') -> if (t1' = t2) then t2' else raise IncorretExpType
         | _ -> raise IncorretExpType
    )
  | Pair(e1, e2) -> 
    ( match TyPair(typeInfer env e1, typeInfer env e2) with
      | TyPair(t1, t2) -> TyPair(t1, t2)
      | _ -> raise IncorretExpType
    )
  | Fst(e) -> 
    (match typeInfer env e with
     | TyPair(t1, _) -> t1
     | _ -> raise IncorretExpType
    )
  | Snd(e) -> 
    (match typeInfer env e with
     | TyPair(_, t2) -> t2
     | _ -> raise IncorretExpType
    )
  (* |  *)