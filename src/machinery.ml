open Ttree
open Context

let rec nil_type = T_var { level = 0; link = nil_type }
let initial_level = 1

(* TODO: maybe open hole and new var point to itself? *)
let is_nil_type type_ = type_ == nil_type

(* TODO: path com pression *)
let rec expand_head type_ =
  match type_ with
  | T_constr _ as type_ -> type_
  | T_arrow _ as type_ -> type_
  | T_var { level = _; link } as type_ -> (
      match is_nil_type link with true -> type_ | false -> expand_head link)

let generic_level = Int.max_int
let is_generic_level level = level == generic_level

let new_hole ctx =
  let level = current_level ctx in
  T_var { level; link = nil_type }

let new_var ctx =
  let level = current_instance ctx in
  T_var { level; link = nil_type }

let is_generic ~level = level >= initial_instance

(* TODO: test zero cost generalization *)
let rec generalize ctx type_ =
  let generalize type_ = generalize ctx type_ in
  match expand_head type_ with
  | T_constr { level = _; var = _ } -> ()
  | T_arrow { param; return } ->
      generalize param;
      generalize return
  | T_var var -> (
      match var.level > current_level ctx with
      | true -> var.link <- new_var ctx
      | false -> ())

let rec instance ctx type_ =
  let instance type_ = instance ctx type_ in
  match expand_head type_ with
  | T_constr { level = _; var = _ } as type_ -> type_
  | T_arrow { param; return } ->
      let param = instance param in
      let return = instance return in
      T_arrow { param; return }
  | T_var var as type_ -> (
      match is_generic ~level:var.level with
      | true ->
          (match var.level = current_instance ctx with
          | true -> ()
          | false ->
              var.level <- current_instance ctx;
              var.link <- new_hole ctx);
          var.link
      | false -> type_)

(* TODO: hole is a bad name, those are existential variables *)
(* TODO: also does escape check and lower the levels *)
let rec occurs_check ~hole_level ~hole_type in_ =
  occurs_check_physical ~hole_level ~hole_type in_

and occurs_check_physical ~hole_level ~hole_type in_ =
  match hole_type == in_ with
  | true -> failwith "occurs check"
  | false -> occurs_check_structural ~hole_level ~hole_type in_

and occurs_check_structural ~hole_level ~hole_type in_ =
  let occurs_check in_ = occurs_check ~hole_level ~hole_type in_ in
  match in_ with
  | T_constr { level; var = _ } -> (
      (* TODO: test this *)
      (* TODO: > vs >=? *)
      match level >= hole_level with
      | true -> failwith "escape check"
      | false -> ())
  | T_arrow { param; return } ->
      occurs_check param;
      occurs_check return
  | T_var in_hole -> in_hole.level <- min hole_level in_hole.level

let rec subtype ~received ~expected = subtype_physical ~received ~expected

and subtype_physical ~received ~expected =
  let received = expand_head received in
  let expected = expand_head expected in
  match received == expected with
  | true -> ()
  | false -> subtype_structural ~received ~expected

and subtype_structural ~received ~expected =
  match (expected, received) with
  | ( T_constr { level = _; var = received },
      T_constr { level = _; var = expected } ) -> (
      match Var.equal received expected with
      | true ->
          (* TODO: this is a bug? It should had stopped at physical equality *)
          ()
      | false -> failwith "constr clash")
  | ( T_arrow { param = received_param; return = received_return },
      T_arrow { param = expected_param; return = expected_return } ) ->
      subtype ~received:expected_param ~expected:received_param;
      subtype ~received:received_return ~expected:expected_return
  | (T_var hole as hole_type), to_ | to_, (T_var hole as hole_type) ->
      occurs_check ~hole_level:hole.level ~hole_type to_;
      hole.link <- to_
  | T_constr _, T_arrow _ | T_arrow _, T_constr _ -> failwith "type clash"

let split_arrow ctx type_ =
  let param = new_hole ctx in
  let return = new_hole ctx in
  (* TODO: probably better to do manually, better error message *)
  let () =
    let expected = T_arrow { param; return } in
    subtype ~received:type_ ~expected
  in
  (param, return)
