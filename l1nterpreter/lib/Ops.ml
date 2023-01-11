module Op = struct
  type operators =
    | Sum
    | Diff
    | Mult
    | Div 
    | Eq
    | Less
    | Leq
    | Greater
    | Geq
    | And
    | Or

  type value = 
    | Numeric of int
    | Boolean of bool
  
  exception IncorretValueType

  let sum (x: value)(y: value) : value = match (x, y) with
    | (Numeric(x), Numeric(y)) -> Numeric(x+y)
    | _ -> raise IncorretValueType
  let diff (x: value)(y: value) : value = match (x, y) with
    | (Numeric(x), Numeric(y)) -> Numeric(x-y)
    | _ -> raise IncorretValueType

  let mult (x: value)(y: value) : value = match (x, y) with
    | (Numeric(x), Numeric(y)) -> Numeric(x*y)
    | _ -> raise IncorretValueType

  let div (x: value)(y: value) : value = match (x, y) with
    | (Numeric(x), Numeric(y)) -> Numeric(x / y)
    | _ -> raise IncorretValueType

  let eq (x: value)(y: value) : value = match (x, y) with
    | (Numeric(x), Numeric(y)) -> Boolean(x = y)
    | _ -> raise IncorretValueType  
  let less (x: value)(y: value) : value = match (x, y) with
    | (Numeric(x), Numeric(y)) -> Boolean(x < y)
    | _ -> raise IncorretValueType
  let leq (x: value)(y: value) : value = match (x, y) with
    | (Numeric(x), Numeric(y)) -> Boolean(x <= y)
    | _ -> raise IncorretValueType
  
  let greater (x: value)(y: value) : value = match (x, y) with
    | (Numeric(x), Numeric(y)) -> Boolean(x > y)
    | _ -> raise IncorretValueType

  let geq (x: value)(y: value) : value = match (x, y) with
    | (Numeric(x), Numeric(y)) -> Boolean(x >= y)
    | _ -> raise IncorretValueType  

  let opAnd (x: value)(y: value) : value = match (x, y) with
    | (Boolean(x), Boolean(y)) -> Boolean(x && y)
    | _ -> raise IncorretValueType
  let opOr (x: value)(y: value) : value = match (x, y) with
    | (Boolean(x), Boolean(y)) -> Boolean(x || y)
    | _ -> raise IncorretValueType  
end