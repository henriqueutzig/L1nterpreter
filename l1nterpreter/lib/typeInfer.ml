include Types;;
include Expressions;;

exception IncorretExpType;;

let rec typeInfer (e: exp) : t =  match e with
  (* Basic expressions *)
  | Num  e -> IntT
  | Bool e -> BoolT
  (* Op expressions *)
  | Op (x1,Sum,x2) -> 
     (match (typeInfer x1, typeInfer x2) with
      (IntT, IntT) -> IntT
      | _ -> raise IncorretExpType)
  | Op (x1,Diff,x2) -> 
    (match (typeInfer x1, typeInfer x2) with
      (IntT, IntT) -> IntT
      | _ -> raise IncorretExpType)
  | Op (x1,Mult,x2) -> 
    (match (typeInfer x1, typeInfer x2) with
      (IntT, IntT) -> IntT
      | _ -> raise IncorretExpType)
  | Op (x1,Div,x2) -> 
    (match (typeInfer x1, typeInfer x2) with
      (IntT, IntT) -> IntT
      | _ -> raise IncorretExpType)
  | Op (x1,Eq,x2) -> 
    (match (typeInfer x1, typeInfer x2) with
      (IntT, IntT) -> BoolT
      | _ -> raise IncorretExpType)
  | Op (x1,Less,x2) -> 
    (match (typeInfer x1, typeInfer x2) with
      (IntT, IntT) -> BoolT
      | _ -> raise IncorretExpType)
  | Op (x1,Leq,x2) -> 
    (match (typeInfer x1, typeInfer x2) with
      (IntT, IntT) -> BoolT
      | _ -> raise IncorretExpType)
  | Op (x1,Greater,x2) -> 
    (match (typeInfer x1, typeInfer x2) with
      (IntT, IntT) -> BoolT
      | _ -> raise IncorretExpType)
  | Op (x1,Geq,x2) -> 
    (match (typeInfer x1, typeInfer x2) with
      (IntT, IntT) -> BoolT
      | _ -> raise IncorretExpType)
  | Op (x1,And,x2) -> 
    (match (typeInfer x1, typeInfer x2) with
      (BoolT, BoolT) -> BoolT
      | _ -> raise IncorretExpType)
  | Op (x1,Or,x2) -> 
    (match (typeInfer x1, typeInfer x2) with
      (BoolT, BoolT) -> BoolT
      | _ -> raise IncorretExpType)
  (* IF expression *)
  | If (e1, e2, e3) ->
    (match (typeInfer e1, typeInfer e2, typeInfer e3) with
      (BoolT, t2, t3) when t2 == t3 -> t2
      | _ -> raise IncorretExpType)
  (* ===> TODO: varible expression <===*)
  (* | Var(x) ->  *)
  | Ap(e1, e2) ->
    (

    )
  | Fn()