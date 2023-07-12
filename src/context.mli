open Ttree

type context
type t = context

val make : unit -> context

(* level *)
val current_level : context -> int
val enter_level : context -> unit
val leave_level : context -> unit

(* instance *)
val initial_instance : int
val current_instance : context -> int
val bump_instance : context -> unit

(* types *)
val lookup_type : context -> var:string -> (Var.t * type_) option
val with_type : context -> var:string -> (Var.t -> 'a) -> 'a

(* values *)
val lookup_value : context -> var:string -> (Var.t * type_) option
val with_value : context -> var:string -> type_ -> (Var.t -> 'a) -> 'a
