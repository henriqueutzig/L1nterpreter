type expType =
  | TyInt
  | TyBool
  | TyFunc of expType * expType
  | TyPair of expType * expType
  | TyList of expType
  | TyNil  of expType
  | TyMaybe of expType
