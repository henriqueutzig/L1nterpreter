type exType =
  | TyInt
  | TyBool
  | TyFunc of exType * exType
  | TyPair
  | TyList
  | TyMaybe
