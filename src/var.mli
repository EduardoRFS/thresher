type var
type t = var [@@deriving show]

val create : string -> var
val equal : var -> var -> bool
val compare : var -> var -> int
val name : var -> string

(* predefined *)
(* Type *)
val type_ : var

module Map : Map.S with type key = t
