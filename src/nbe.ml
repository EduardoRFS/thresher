type var = string
type level = int

let initial_level = 1

(*
  F = Formation
  I = Introduction
  E = Elimination
*)
type term = Term of { desc : term_desc; type_ : value; loc : Location.t }

and term_desc =
  | T_type
  | T_var of var
  | T_let of var * term * term
  (* boxing *)
  | TF_box of term
  | TI_box of term
  | TE_unbox of term
  (* forall *)
  | TF_forall of var * term * term
  | TI_lambda of var * term
  | TE_apply of term * term
  (* pair *)
  | TF_exists of var * term * term
  | TI_pair of term * term
  | TE_fst of term
  | TE_snd of term

(* elimination in values are neutrals *)
and value =
  | Value of {
      mutable desc : value_desc;
      mutable level : level;
      mutable link : value option;
    }

and value_desc =
  | V_type
  | V_var
  | V_hole
  | VF_box of value
  | VI_box of value
  | VE_unbox of value
  | VF_forall of value * closure
  | VI_lambda of closure
  | VE_apply of value * value
  | VF_exists of value * closure
  | VI_pair of value * value
  | VE_fst of value
  | VE_snd of value

and env =
  (* _ *)
  | L_hole
  (* x = M; L *)
  | L_let of var * value * env

and closure =
  (* L<x = _; M> *)
  | Closure of env * var * term

let t_desc term =
  let (Term { desc; type_ = _; loc = _ }) = term in
  desc

let v_new = fun level desc -> Value { desc; level; link = None }

let rec v_repr value =
  let (Value { desc = _; level = _; link }) = value in
  match link with Some value_link -> v_repr value_link | None -> value

let v_same left right = v_repr left == v_repr right

let rec v_set_link value ~to_ =
  let (Value value) = v_repr value in
  value.link <- Some to_

let v_level value =
  (* TODO: this could be done better if we had the internal stuff *)
  let (Value { desc = _; level; link = _ }) = v_repr value in
  level

let v_set_level value level =
  let (Value value) = v_repr value in
  value.level <- level

let v_desc value =
  let (Value { desc; level = _; link = _ }) = v_repr value in
  desc

let rec e_level env =
  match env with
  | L_hole -> initial_level
  | L_let (_var, arg, env) -> max (v_level arg) (e_level env)

let c_level closure =
  let (Closure (env, _bound, _term)) = closure in
  e_level env

(* helpers *)
let v_type = v_new initial_level V_type
let v_var level = v_new level @@ V_var
let v_hole level = v_new level @@ V_hole

let vf_box type_ =
  let level = v_level type_ in
  v_new level @@ VF_box type_

let vi_box content =
  let level = v_level content in
  v_new level @@ VI_box content

let ve_unbox box =
  let level = v_level box in
  v_new level @@ VE_unbox box

let vf_forall param body =
  let level = max (v_level param) (c_level body) in
  v_new level @@ VF_forall (param, body)

let vi_lambda body =
  let level = c_level body in
  v_new level @@ VI_lambda body

let ve_apply funct arg =
  let level = max (v_level funct) (v_level arg) in
  v_new level @@ VE_apply (funct, arg)

let vf_exists param body =
  let level = max (v_level param) (c_level body) in
  v_new level @@ VF_exists (param, body)

let vi_pair fst snd =
  let level = max (v_level fst) (v_level snd) in
  v_new level @@ VI_pair (fst, snd)

let ve_fst pair =
  let level = v_level pair in
  v_new level @@ VE_fst pair

let ve_snd pair =
  let level = v_level pair in
  v_new level @@ VE_snd pair

(* helpers *)

(* L<M> |-> N *)
let rec eval env term =
  match t_desc term with
  | T_type -> v_type
  | T_var var -> eval_var env var
  | T_let (var, arg, body) ->
      let arg = eval env arg in
      let env = L_let (var, arg, env) in
      eval env body
  | TF_box type_ ->
      let type_ = eval env type_ in
      vf_box type_
  | TI_box content ->
      let content = eval env content in
      vi_box content
  | TE_unbox box ->
      let box = eval env box in
      eval_unbox box
  | TF_forall (var, param, body) ->
      let param = eval env param in
      let body = Closure (env, var, body) in
      vf_forall param body
  | TI_lambda (var, body) ->
      let body = Closure (env, var, body) in
      vi_lambda body
  | TE_apply (funct, arg) ->
      let funct = eval env funct in
      let arg = eval env arg in
      eval_apply funct arg
  | TF_exists (var, param, body) ->
      let param = eval env param in
      let body = Closure (env, var, body) in
      vf_exists param body
  | TI_pair (fst, snd) ->
      let fst = eval env fst in
      let snd = eval env snd in
      let level = max (v_level fst) (v_level snd) in
      v_new level @@ VI_pair (fst, snd)
  | TE_fst pair ->
      let pair = eval env pair in
      eval_fst pair
  | TE_snd pair ->
      let pair = eval env pair in
      eval_snd pair

and eval_closure closure arg =
  let (Closure (env, var, body)) = closure in
  let env = L_let (var, arg, env) in
  eval env body

and eval_var env var =
  (* x = M; L<x> |-> M *)
  match env with
  | L_hole -> failwith "eval_var: unbound variable"
  | L_let (x, arg, env) -> (
      match x = var with true -> arg | false -> eval_var env var)

and eval_apply funct arg =
  (* (x => M)(N) *)
  match v_desc funct with
  | VI_lambda body -> eval_closure body arg
  | _ -> ve_apply funct arg

and eval_unbox box =
  (* unbox(box(M)) |-> M *)
  match v_desc box with
  | VI_box content -> content
  | _ -> ve_unbox box

and eval_fst pair =
  (* [M, N].0 |-> N *)
  match v_desc pair with
  | VI_pair (fst, _snd) -> fst
  | _ -> ve_fst pair

and eval_snd pair =
  (* [M, N].1 |-> M *)
  match v_desc pair with
  | VI_pair (_fst, snd) -> snd
  | _ -> ve_snd pair

(* unification *)
let rec unify_check ~at ~hole to_ =
  (* TODO: explain why this works *)
  match at > v_level to_ with
  | true -> ()
  | false ->
      (* lowering *)
      v_set_level to_ at;
      unify_check_desc ~at ~hole to_

and unify_check_desc ~at ~hole to_ =
  match v_desc to_ with
  | V_type -> ()
  | V_var -> failwith "escape check"
  | V_hole -> (
      match v_same hole to_ with
      | true -> ()
      | false -> unify_check ~at ~hole to_)
  | VF_box type_ -> unify_check ~at ~hole type_
  | VI_box content -> unify_check ~at ~hole content
  | VE_unbox box -> unify_check ~at ~hole box
  | VF_forall (param, body) ->
      unify_check ~at ~hole param;
      unify_check_closure ~at ~hole body
  | VI_lambda body -> unify_check_closure ~at ~hole body
  | VE_apply (funct, arg) ->
      unify_check ~at ~hole funct;
      unify_check ~at ~hole arg
  | VF_exists (fst, snd) ->
      unify_check ~at ~hole fst;
      unify_check_closure ~at ~hole snd
  | VI_pair (fst, snd) ->
      unify_check ~at ~hole fst;
      unify_check ~at ~hole snd
  | VE_fst pair -> unify_check ~at ~hole pair
  | VE_snd pair -> unify_check ~at ~hole pair

and unify_check_closure ~at ~hole closure =
  let (Closure (env, _bound, _term)) = closure in
  unify_check_env ~at ~hole env

and unify_check_env ~at ~hole env =
  match env with
  | L_hole -> ()
  | L_let (_var, arg, env) ->
      unify_check ~at ~hole arg;
      unify_check_env ~at ~hole env

let unify_check ~hole value =
  let at = v_level value in
  unify_check_desc ~at ~hole value

let rec unify left right =
  match v_same left right with
  | true -> ()
  | false ->
      (* TODO: explain this *)
      v_set_link left ~to_:right;
      unify_desc left right

and unify_desc received expected =
  match (v_desc @@ received, v_desc expected) with
  | V_hole, _ -> unify_check ~hole:received expected
  | _, V_hole -> unify_check ~hole:expected received
  | VF_box received_type, VF_box expected_type ->
      unify received_type expected_type
  | VI_box received_content, VI_box expected_content ->
      unify received_content expected_content
  | VE_unbox received_box, VE_unbox expected_box ->
      unify received_box expected_box
  | ( VF_forall (received_param, received_body),
      VF_forall (expected_param, expected_body) ) ->
      (* no contravariance? *)
      unify received_param expected_param;
      unify_closure received_body expected_body
  | VI_lambda received_body, VI_lambda expected_body ->
      unify_closure received_body expected_body
  | ( VE_apply (received_funct, received_arg),
      VE_apply (expected_funct, expected_arg) ) ->
      unify received_funct expected_funct;
      unify received_arg expected_arg
  | ( VF_exists (received_param, received_body),
      VF_exists (expected_param, expected_body) ) ->
      unify received_param expected_param;
      unify_closure received_body expected_body
  | VI_pair (received_fst, received_snd), VI_pair (expected_fst, expected_snd)
    ->
      unify received_fst expected_fst;
      unify received_snd expected_snd
  | VE_fst received_pair, VE_fst expected_pair ->
      unify received_pair expected_pair
  | VE_snd received_pair, VE_snd expected_pair ->
      unify received_pair expected_pair
  | _, _ -> failwith "type clash"

and unify_closure received expected =
  let level = max (c_level received) (c_level expected) in
  let skolem = v_var level in
  let received = eval_closure received skolem in
  let expected = eval_closure expected skolem in
  unify received expected

(* typer *)
module AST = struct
  type var = string

  type term = Term of { desc : term_desc; loc : Location.t }

  and term_desc =
    | T_type
    | T_var of var
    | T_let of var * term * term
    (* boxing *)
    | TF_box of term
    | TI_box of term
    | TE_unbox of term
    (* forall *)
    | TF_forall of var * term * term
    | TI_lambda of var * term
    | TE_apply of term * term
    (* pair *)
    | TF_exists of var * term * term
    | TI_pair of term * term
    | TE_fst of term
    | TE_snd of term
end

type context = { level : level; env : env; vars : (AST.var * value) list }

let rec lookup ctx var =
  let var = List.find_map (fun (x, type_) -> _) in
  _

let enter_var : context -> AST.var -> type_:value -> value -> context = _
let enter_let : context -> AST.var -> type_:value -> value -> context = _
let subtype ~received ~expected : unit = _
let eval : context -> term -> value = _
let v_var : context -> value = _
let v_hole : context -> value = _
let split_vf_box : value -> value = _
let split_vf_forall : value -> value * closure = _
let split_vf_exists : value -> value * closure = _

let rec infer_term ctx term =
  let expected = v_hole ctx in
  (check_term ctx term expected, expected)

and check_term ctx term expected =
  let (AST.Term { desc; loc }) = term in
  let t_wrap desc = Term { desc; type_ = expected; loc } in
  match desc with
  | T_type ->
      subtype ~received:v_type ~expected;
      t_wrap T_type
  | T_var var ->
      let received = lookup ctx var in
      subtype ~received ~expected;
      t_wrap @@ T_var var
  | T_let (var, arg, body) ->
      let arg, arg_type = infer_term ctx arg in
      let body =
        (* TODO: lazy eval arg *)
        let arg = eval ctx arg in
        let ctx = enter_let ctx var arg ~type_:arg_type in
        check_term ctx body expected
      in
      t_wrap @@ T_let (var, arg, body)
  | TF_box type_ ->
      subtype ~received:v_type ~expected;
      check_term ctx type_ v_type
  | TI_box content ->
      let content_type = split_vf_box expected in
      subtype ~received:content_type ~expected;
      let content = check_term ctx content content_type in
      t_wrap @@ TI_box content
  | TE_unbox box ->
      let box_type = vf_box expected in
      let box = check_term ctx box box_type in
      (* box is transparent *)
      t_wrap @@ TE_unbox box
  | TF_forall (var, param, body) ->
      subtype ~received:v_type ~expected;
      let param = check_term ctx param v_type in
      let body =
        let param = eval ctx param in
        let skolem = v_var ctx in
        let ctx = enter_var ctx var skolem ~type_:param in
        check_term ctx body v_type
      in
      t_wrap @@ TF_forall (var, param, body)
  | TI_lambda (var, body) ->
      let param_type, body_type = split_vf_forall expected in
      let body =
        let skolem = v_var ctx in
        let ctx = enter_var ctx var skolem ~type_:param_type in
        let body_type = eval_closure body_type skolem in
        check_term ctx body body_type
      in
      t_wrap @@ TI_lambda (var, body)
  | TE_apply (funct, arg) ->
      let funct, forall = infer_term ctx funct in
      let param_type, body_type = split_vf_forall forall in
      let arg = check_term ctx arg param_type in
      let () =
        let arg = eval ctx arg in
        let received = eval_closure body_type arg in
        subtype ~received ~expected
      in
      t_wrap @@ TE_apply (funct, arg)
  | TF_exists (var, fst, snd) ->
      subtype ~received:v_type ~expected;
      let fst = check_term ctx fst v_type in
      let snd =
        let fst = eval ctx fst in
        let skolem = v_var ctx in
        let ctx = enter_var ctx var skolem ~type_:fst in
        check_term ctx snd v_type
      in
      t_wrap @@ TF_exists (var, fst, snd)
  | TI_pair (fst, snd) ->
      let fst_type, snd_type = split_vf_exists expected in
      let fst = check_term ctx fst fst_type in
      let snd =
        let fst = eval ctx fst in
        let snd_type = eval_closure snd_type fst in
        check_term ctx snd snd_type
      in
      t_wrap @@ TI_pair (fst, snd)
  | TE_fst pair ->
      let pair, exists = infer_term ctx pair in
      let () =
        let fst_type, _snd_type = split_vf_exists exists in
        subtype ~received:fst_type ~expected
      in
      t_wrap @@ TE_fst pair
  | TE_snd pair ->
      let pair, exists = infer_term ctx pair in
      let () =
        let _fst_type, snd_type = split_vf_exists exists in
        let pair = eval ctx pair in
        let fst = eval_fst pair in
        let snd_type = eval_closure snd_type fst in
        subtype ~received:snd_type ~expected
      in
      t_wrap @@ TE_snd pair

module Subtype = struct
  type bot = [ `bot ]
  type nat = [ `nat | bot ]
  type int = [ `int | nat ]
  type float = [ `float | bot ]
  type number = [ int | float ]
  type 'a x = [< `bot | `nat > `bot ] as 'a

  let x : _ x = `bot
  let add : ([< int ] as 'a) -> 'a -> 'a = assert false
  let zero : [< `bot | `nat > `bot `nat ] = `nat
  let minus_one : int = `int
  let x = add zero zero

  let zero : 'a. ([> nat ] as 'a) =
    let (#nat as zero) = zero in
    zero
end
