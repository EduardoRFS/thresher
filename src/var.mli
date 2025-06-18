type var
type t = var [@@deriving eq, ord, show]

val create : string -> var
val name : var -> string
