type expType =
  | TyInt
  | TyBool
  | TyFunc of expType * expType
  | TyPair
  | TyList of expType
  | TyMaybe
