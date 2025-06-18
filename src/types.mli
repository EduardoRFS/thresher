module Tag : sig
  type tag
  type t = tag [@@deriving eq, show]

  val initial_level : tag
  val initial_generic : tag
  val next : tag -> tag
end

type type_

and type_desc = private
  | T_hole of { bound : bound }
  | T_constr of { constr : constr; args : type_ list }
  (* TODO: return and snd don't have the same kind *)
  | T_arrow of { param : type_; return : type_ }
  | T_pair of { fst : type_; snd : type_ }
  (* TODO: this could be part of the tag *)
  | T_break of { type_ : type_ }

and bound =
  | B_none
  | B_enum of { closed : bool; rows : rows }
  | B_record of { closed : bool; rows : rows }

and constr

and constr_body = private
  | C_opaque
  | C_alias of { body : type_ }
  | C_record of { rows : rows }
  | C_enum of { rows : rows }
[@@deriving show]

(* TODO: rename everything to fields or rows *)
and rows

(* types *)
val desc : type_ -> type_desc
val new_hole : tag:Tag.t -> type_
val new_hole_bound_by_open_record : tag:Tag.t -> (string * type_) list -> type_
val new_hole_bound_by_record : tag:Tag.t -> (string * type_) list -> type_
val new_deep_hole : tag:Tag.t -> type_

(* TODO: arith clash *)
val new_constr : constr:constr -> args:type_ list -> type_
val new_arrow : params:type_ list -> return:type_ -> type_
val new_tuple : fields:type_ list -> type_

(* constructors *)

val new_alias : body:type_ -> constr
(* val new_row : name:string -> payload:type_ -> row
   val new_record : rows:row list -> constr
   val new_enum : rows:row list -> constr *)

(* machinery *)
val generalize : current_level:Tag.t -> type_ -> unit
val instance : current_instance:Tag.t -> current_level:Tag.t -> type_ -> type_

val unify :
  current_instance:Tag.t ->
  current_level:Tag.t ->
  received:type_ ->
  expected:type_ ->
  unit
