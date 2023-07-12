open Ttree

(* TODO: hash map*)
module Vars = Hashtbl.Make (String)

type context = {
  mutable level : int;
  mutable instance : int;
  types : (Var.t * type_) Vars.t;
  values : (Var.t * type_) Vars.t;
}

type t = context

let initial_instance = Int.max_int lsr 1

let make () =
  let level = 0 in
  let instance = initial_instance in
  (* TODO: choose size *)
  let size = 128 in
  let types = Vars.create size in
  let values = Vars.create size in
  { level; instance; types; values }

(* levels *)
let current_level ctx = ctx.level
let enter_level ctx = ctx.level <- ctx.level + 1
let leave_level ctx = ctx.level <- ctx.level - 1

(* instance *)
let current_instance ctx = ctx.instance

let bump_instance ctx =
  (* TODO: overloading check? 32bits? *)
  ctx.instance <- ctx.instance + 1

(* types *)
let lookup_type ctx ~var = Vars.find_opt ctx.types var

let with_type ctx ~var:name f =
  let var = Var.create name in
  let type_ =
    let level = current_level ctx in
    T_constr { level; var }
  in
  Vars.add ctx.types name (var, type_);
  let value = f var in
  Vars.remove ctx.types name;
  value

(* values *)
let lookup_value ctx ~var = Vars.find_opt ctx.values var

let with_value ctx ~var:name type_ f =
  let var = Var.create name in
  Vars.add ctx.values name (var, type_);
  let value = f var in
  Vars.remove ctx.values name;
  value
