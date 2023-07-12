[@@@ocaml.warning "-unused-constructor"]

open Ttree

module Ptree = struct
  open Format

  type term =
    | PT_var of { var : string }
    | PT_var_full of { level : int; var : Var.t }
    | PT_uni_var of { var : string }
    | PT_uni_var_full of { level : int; var : string }
    | PT_ex_var of { var : string }
    | PT_ex_var_full of { level : int; var : string }
    (* TODO: PT_link *)
    | PT_arrow of { param : term; return : term }

  let pp_term_syntax ~pp_funct ~pp_atom fmt term =
    match term with
    | PT_var { var } -> fprintf fmt "%s" var
    | PT_var_full { level; var } -> fprintf fmt "%a/%d" Var.pp var level
    | PT_uni_var { var } -> fprintf fmt "'%s" var
    | PT_uni_var_full { level; var } -> fprintf fmt "'%s/%d" var level
    | PT_ex_var { var } -> fprintf fmt "'_%s" var
    | PT_ex_var_full { level; var } -> fprintf fmt "'_%s/%d" var level
    | PT_arrow { param; return } ->
        fprintf fmt "%a -> %a" pp_atom param pp_funct return

  type prec = Funct | Atom

  let rec pp_term prec fmt term =
    let pp_funct fmt term = pp_term Funct fmt term in
    let pp_atom fmt term = pp_term Atom fmt term in
    match (term, prec) with
    | ( ( PT_var _ | PT_var_full _ | PT_uni_var _ | PT_uni_var_full _
        | PT_ex_var _ | PT_ex_var_full _ ),
        (Funct | Atom) )
    | PT_arrow _, Funct ->
        pp_term_syntax ~pp_funct ~pp_atom fmt term
    | PT_arrow _, Atom -> fprintf fmt "(%a)" pp_funct term

  let pp_term fmt term = pp_term Funct fmt term
end

type var_mode = Var_name | Var_full
type config = { var_mode : var_mode }

module Vars = Hashtbl.Make (struct
  type t = type_

  let equal a b = a == b

  (* TODO: this is only safe because of implementation details *)
  external type_to_int : type_ -> int = "%identity"

  let hash type_ = type_to_int type_ lor 0
end)

let rec ptree_of_type next_id vars config type_ =
  let open Ptree in
  let open Machinery in
  let ptree_of_type term = ptree_of_type next_id vars config term in
  match Machinery.expand_head type_ with
  | T_constr { level; var } -> (
      match config.var_mode with
      | Var_name ->
          let var = Var.name var in
          PT_var { var }
      | Var_full -> PT_var_full { level; var })
  | T_arrow { param; return } ->
      let param = ptree_of_type param in
      let return = ptree_of_type return in
      PT_arrow { param; return }
  (* TODO: option for link and inst *)
  | T_var { level; link = _ } as type_ -> (
      match Vars.find_opt vars type_ with
      | Some term -> term
      | None ->
          let id = !next_id in
          next_id := id + 1;
          let var = Format.sprintf "x%d" id in
          let term =
            match is_generic ~level with
            | true -> (
                match config.var_mode with
                | Var_name -> PT_uni_var { var }
                | Var_full -> PT_uni_var_full { level; var })
            | false -> (
                match config.var_mode with
                | Var_name -> PT_ex_var { var }
                | Var_full -> PT_ex_var_full { level; var })
          in
          Vars.add vars type_ term;
          term)

let config = { var_mode = Var_name }

let pp_type fmt term =
  let next_id = ref 0 in
  let vars = Vars.create 4 in
  let pterm = ptree_of_type next_id vars config term in
  Ptree.pp_term fmt pterm
