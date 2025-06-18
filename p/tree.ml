type expr =
  | Literal of int
  | Add of (expr * expr)
  | Sub of (expr * expr)
  | Mul of (expr * expr)
  | Div of (expr * expr)
[@@deriving show]
