type t =
  | IntT
  | BoolT
  | FuncT of t * t
  | PairT
  | ListT 
  | MaybeT
