(* TODO: keep track of unification diff, add id to hole *)
(* TODO: reversible reductions *)
(* TODO: explain link, in generalize, instance and expand *)
(* TODO: every constructor introduces a forall  *)
(* TODO: put names on everything *)
(* TODO: abbreviations through link? *)
(* TODO: shared abbreviations? *)
(* TODO: named tuples *)

module Level = struct
  type level = int
  and t = level [@@deriving eq, show]

  let equal = Int.equal
  let null = 0
  let generic = 1_000_000_000
  let instance = generic + 1
  let linked = instance + 1
  let initial = 2
  let is_weak level = level < generic
  let is_generic level = equal level generic
  let is_instance level = equal level instance
  let next level = level + 0b10

  (* TODO: generic is also considered dead *)
  let is_dead_region ~current level = level > current

  let would_escape ~(from : level) ~(to_ : level) =
    (* TODO: should both be weak? *)
    from > to_

  let min (left : level) (right : level) =
    (* TODO: should both be weak? *)
    min left right

  let max (left : level) (right : level) = max left right
end

module String_map = Map.Make (String)
module Var_map = Map.Make (Var)

(* TODO: improve this *)
module Hole = Var

module M = struct
  type term = Term of { desc : term_desc; loc : Location.t [@opaque] }

  and term_desc =
    | T_type
    | T_data
    | T_annot of { term : term; annot : type_ }
    | T_let of { bound : pat; arg : term; body : term }
    | T_var of { var : Var.t }
    | T_hole of { hole : Hole.t }
    | TF_box of { type_ : type_ }
    | TI_box of { content : term }
    | TE_unbox of { box : term }
end

module Core = struct
  (* TODO: mutation *)
  (* TODO: recursion *)
  (* TODO: *)
  (*
    T = Term
    F = Formation
    I = Introduction
    E = Elimination
    P = Pattern
    L = Label
    C = Cases
  *)
  type term = Term of { desc : term_desc; loc : Location.t [@opaque] }

  and term_desc =
    (* core *)
    | T_type
    | T_data
    | T_annot of { term : term; annot : type_ }
    | T_let of { bound : pat; arg : term; body : term }
    | T_var of { var : Var.t }
    (* boxing *)
    | TF_box of { type_ : type_ }
    | TI_box of { content : term }
    | TE_unbox of { box : term }
    (* nominal *)
    | TF_nominal of { type_ : type_ }
    | TI_nominal of { content : term }
    (* functions *)
    | TF_forall of { bound : pat; param : type_; body : type_ }
    | TI_lambda of { bound : pat; body : term }
    | TE_apply of { funct : term; arg : term }
    (* pairs *)
    | TF_unit
    | TI_unit
    | TF_exists of { bound : pat; left : type_; rest : rows }
    (* TODO: fst and snd *)
    | TI_pair of { left : term; rest : rows }
    (* TODO: M.n *)
    | TE_fst of { pair : term }
    | TE_snd of { pair : term }
    (* records *)
    | TF_empty
    | TI_empty
    (* TODO: this seems bad *)
    | TF_record of { label : label; bound : pat; left : type_; rest : rows }
    | TI_record of { label : label; bound : pat; left : term; rest : rows }
    | TE_field of { record : term; label : label }
    (* enum *)
    | TF_never
    | TF_enum of { label : label; left : type_; rest : rows }
    | TI_enum of { label : label; content : term }
    | TE_match of { pred : term; cases : case list }

  and pat = Pat of { desc : pat_desc; loc : Location.t [@opaque] }

  and pat_desc =
    | P_annot of { pat : pat; annot : type_ }
    | P_var of { var : Var.t }
    | P_alias of { pat : pat; as_ : Var.t }
    | P_or of { left : pat; right : pat }
    | P_box of { content : pat }
    | P_unit
    | P_pair of { left : pat; rest : pat }
    | P_empty
    | P_record of { label : label; left : type_; rest : pat }
    | P_never
    | P_enum of { label : label; content : pat }

  and rows = term
  and type_ = term
  and label = Label of { content : string; loc : Location.t }
  and case = Case of { cond : pat; guard : term; then_ : term }
end

module Value = struct
  open Core

  (* TODO: id for debugging *)
  (* TODO: location in the value? *)
  type value = {
    mutable desc : value_desc;
    mutable level : Level.t;
    mutable link : value; [@opaque]
  }

  and value_desc =
    | V_type
    | V_data
    | V_var
    | V_hole
    (* laziness *)
    | VF_lazy of { type_ : value }
    | VI_thunk of { env : env; content : term }
    | VE_force of { thunk : value }
    (* boxing *)
    | VF_box of { type_ : value }
    | VI_box of { content : value }
    | VE_unbox of { box : value }
    (* nominality *)
    | VF_nominal of { type_ : value }
    | VI_nominal of { content : value }
    (* functions *)
    | VF_forall of { param : value; body : closure }
    | VI_lambda of { body : closure }
    | VE_apply of { funct : value; arg : value }
    (* pairs *)
    | VF_unit
    | VI_unit
    | VF_exists of { left : value; rest : closure }
    | VI_pair of { left : value; rest : value }
    | VE_fst of { pair : value }
    | VE_snd of { pair : value }
    (* records *)
    | VF_empty
    | VI_empty
    | VF_record of { label : label; left : value; rest : closure }
    | VI_record of { label : label; left : value; rest : value }
    | VE_field of { record : value; label : label }
    (* enum *)
    | VF_never
    | VF_enum of { label : label; left : value; rest : value }
    | VI_enum of { label : label; content : value }
    (* TODO: cases here? *)
    | VE_match of { pred : value; cases : cases }

  and env = Env of { level : Level.t; vars : value Var_map.t }

  (* TODO: like a thunk but there is free vars in term *)
  and closure = Closure of env * pat * term
  and cases = Cases of env * case list

  (* TODO: can this be done in Grain? *)
  let rec null = { desc = V_hole; level = Level.null; link = null }

  (* physical equality *)
  let is_null value = value == null

  (* TODO: same vs equal *)

  let rec repr type_ =
    let { desc = _; level = _; link } = type_ in
    match is_null link with true -> type_ | false -> repr link

  let repr type_ =
    let { desc = _; level = _; link } = type_ in
    match is_null link with
    | true -> type_
    | false ->
        let final = repr link in
        type_.link <- final;
        final

  let set_link type_ ~to_ =
    (* TODO: clear the desc? *)
    (repr type_).link <- to_

  let set_level type_ ~level = (repr type_).level <- level
  let v_same left right = repr left == repr right
  let v_desc type_ = (repr type_).desc
  let v_level type_ = (repr type_).level

  let append env var value =
    let (Env { level; vars }) = env in
    let level = Level.max level (v_level value) in
    let vars = Var_map.add var value vars in
    Env { level; vars }

  let lookup env var =
    let (Env { level = _; vars }) = env in
    match Var_map.find_opt var vars with
    | Some value -> value
    | None ->
        failwith @@ Format.asprintf "lookup: variable %a not found" Var.pp var

  let e_level env =
    let (Env { level; vars = _ }) = env in
    level

  let e_iter env f =
    let (Env { level = _; vars }) = env in
    Var_map.iter f vars

  let c_level closure =
    let (Closure (env, _bound, _term)) = closure in
    e_level env

  let new_value : Level.t -> value_desc -> value =
   fun level desc -> { desc; level; link = null }

  (* TODO: check all the levels in the constructors *)
  let vf_type = new_value Level.initial V_type
  let vf_data = new_value Level.initial V_data

  let vf_box type_ =
    let level = v_level type_ in
    new_value level @@ VF_box { type_ }

  let vi_box content =
    let level = v_level content in
    new_value level @@ VI_box { content }

  let ve_unbox box =
    let level = v_level box in
    new_value level @@ VE_unbox { box }

  let vf_forall param body =
    let level = Level.max (v_level param) (c_level body) in
    new_value level @@ VF_forall { param; body }

  let vi_lambda body =
    let level = c_level body in
    new_value level @@ VI_lambda { body }

  let ve_apply funct arg =
    let level = Level.max (v_level funct) (v_level arg) in
    new_value level @@ VE_apply { funct; arg }

  let vf_unit = new_value Level.initial VF_unit
  let vi_unit = new_value Level.initial VI_unit

  let vf_exists left rest =
    let level = Level.max (v_level left) (c_level rest) in
    new_value level @@ VF_exists { left; rest }

  let vi_pair left rest =
    let level = Level.max (v_level left) (v_level rest) in
    new_value level @@ VI_pair { left; rest }

  let ve_fst pair =
    let level = v_level pair in
    new_value level @@ VE_fst { pair }

  let ve_snd pair =
    let level = v_level pair in
    new_value level @@ VE_snd { pair }

  let vf_empty = new_value Level.initial VF_empty
  let vi_empty = new_value Level.initial VI_empty

  let vf_record label left rest =
    let level = Level.max (v_level left) (c_level rest) in
    new_value level @@ VF_record { label; left; rest }

  let vi_record label left rest =
    let level = Level.max (v_level left) (v_level rest) in
    new_value level @@ VI_record { label; left; rest }

  let ve_field record label =
    let level = v_level record in
    new_value level @@ VE_field { record; label }

  let vf_never = new_value Level.initial VF_never

  let vf_enum label left rest =
    let level = Level.max (v_level left) (v_level rest) in
    new_value level @@ VF_enum { label; left; rest }

  let vi_enum label content =
    let level = v_level content in
    new_value level @@ VI_enum { label; content }

  let thunk : env -> term -> value = _
  let closure : env -> pat -> term -> closure = _
end

module Machinery = struct
  open Core
  open Value

  let instance ~at value = _
  and instance_desc ~at value = _

  exception
    Pattern_match_failure of {
      loc : Location.t;
      env : env;
      arg : value;
      pat : pat;
    }

  let rec eval env term =
    (* TODO: elimination of holes? *)
    let (Term { desc; loc }) = term in
    match desc with
    | T_type -> vf_type
    | T_data -> vf_data
    | T_annot { term; annot = _ } -> eval env term
    | T_let { bound; arg; body } ->
        let arg = eval env arg in
        let env = eval_pat env arg bound in
        eval env body
    | T_var { var } -> lookup env var
    | T_hole _ -> _
    | TF_box { type_ } ->
        let type_ = eval_lazy env type_ in
        vf_box type_
    | TI_box { content } ->
        let content = eval env content in
        vi_box content
    | TE_unbox { box } -> (
        let box = eval_force env box in
        match v_desc box with
        | VI_box { content } -> content
        | _ -> ve_unbox box)
    | TF_nominal { type_ } -> _
    | TI_nominal { content } -> _
    | TF_forall { bound; param; body } ->
        let param = eval_lazy env param in
        let body = closure env bound body in
        vf_forall param body
    | TI_lambda { bound; body } ->
        let body = closure env bound body in
        vi_lambda body
    | TE_apply { funct; arg } -> (
        let funct = eval_force env funct in
        let arg = eval env arg in
        match v_desc funct with
        | VI_lambda { body } -> eval_closure ~loc arg body
        | _ -> ve_apply funct arg)
    | TF_unit -> vf_unit
    | TI_unit -> vi_unit
    | TF_exists { bound; left; rest } ->
        let left = eval_lazy env left in
        let rest = closure env bound rest in
        vf_exists left rest
    | TI_pair { left; rest } ->
        let left = eval env left in
        let rest = eval env rest in
        vi_pair left rest
    | TE_fst { pair } -> (
        let pair = eval_force env pair in
        match v_desc pair with
        | VI_pair { left; rest = _ } -> left
        | _ -> ve_fst pair)
    | TE_snd { pair } -> (
        let pair = eval_force env pair in
        match v_desc pair with
        | VI_pair { left = _; rest } -> rest
        | _ -> ve_snd pair)
    | TF_empty -> vf_empty
    | TI_empty -> vi_empty
    | TF_record { label; bound; left; rest } ->
        let left = eval_lazy env left in
        let rest = closure env bound rest in
        vf_record label left rest
    | TI_record { label; bound; left; rest } ->
        let left = eval env left in
        let env = eval_pat env left bound in
        let rest = eval env rest in
        vi_record label left rest
    | TE_field { record; label } ->
        let record = eval_force env record in
        _
    | TF_never -> vf_never
    | TF_enum { label; left; rest } ->
        let left = eval_lazy env left in
        let rest = eval_lazy env rest in
        vf_enum label left rest
    | TI_enum { label; content } ->
        let content = eval env content in
        vi_enum label content
    | TE_match { pred; cases } ->
        let pred = eval env pred in
        eval_match env pred cases

  and eval_pat env arg pat =
    match test_pat env arg pat with Some env -> env | None -> _

  and test_pat env arg pat =
    let ( let* ) = Option.bind in
    let (Pat { desc; loc }) = pat in
    match desc with
    | P_annot { pat; annot = _ } -> test_pat env arg pat
    | P_var { var } -> _
    | P_alias { pat; as_ } -> _
    | P_or { left; right } -> (
        match test_pat env arg left with
        | Some env -> Some env
        | None -> test_pat env arg right)
    | P_box { content } ->
        let content_arg =
          match v_desc arg with
          | VI_box { content } -> content
          | _ -> ve_unbox arg
        in
        test_pat env content_arg content
    (* TODO: weird to ignore the arg *)
    | P_unit -> Some env
    | P_pair { left; rest } ->
        let left_arg, rest_arg =
          match v_desc arg with
          | VI_pair { left; rest } -> (left, rest)
          | _ -> (ve_fst arg, ve_snd arg)
        in
        let* env = test_pat env left_arg left in
        test_pat env rest_arg rest
    (* TODO: also ignore the arg? *)
    | P_empty -> Some env
    | P_record { label; left; rest } -> _
    | P_never -> Some env
    | P_enum { label = expected_label; content = pat } -> (
        match v_desc arg with VI_enum { label; content } -> _ | _ -> _)

  and eval_lazy env term = _
  and eval_force env term = _

  (* TODO: better name for this function *)
  and eval_closure arg body =
    (* TODO: should this location be coming from here? *)
    let (Closure (env, bound, body)) = body in
    let env = eval_pat env arg bound in
    eval env body

  and eval_match ~loc env pred cases = match cases with [] -> _ | _ :: _ -> _

  let rec unify_check ~at ~hole (to_ : value) =
    match Level.(at > v_level to_) with
    | true -> ()
    | false ->
        (* lowering *)
        (* TODO: logs for setting level and linking? *)
        set_level to_ ~level:at;
        unify_check_desc ~at ~hole to_

  and unify_check_desc ~at ~hole to_ =
    match v_desc to_ with
    | V_thunk { env; term = _ } ->
        (* TODO: maybe evaluate this? *)
        unify_check_env ~at ~hole env
    | V_type -> failwith "unify_check: V_type should never happen"
    | V_data -> failwith "unify_check: V_data should never happen"
    | V_var -> failwith "unify_check: var would escape"
    | V_hole -> (
        match v_same to_ hole with
        | true -> failwith "unify_check: hole would occurs in itself"
        | false -> ())
    | VF_box { type_ } -> unify_check ~at ~hole type_
    | VI_box { content } -> unify_check ~at ~hole content
    | VE_unbox { box } -> unify_check ~at ~hole box
    | VF_nominal { type_ } -> unify_check ~at ~hole type_
    | VI_nominal { content } -> unify_check ~at ~hole content
    | VF_forall { param; body } ->
        unify_check ~at ~hole param;
        unify_check_closure ~at ~hole body
    | VI_lambda { body } -> unify_check_closure ~at ~hole body
    | VE_apply { funct; arg } ->
        unify_check ~at ~hole funct;
        unify_check ~at ~hole arg
    | VF_unit -> failwith "unify_check: VF_unit should never happen"
    | VI_unit -> failwith "unify_check: VI_unit should never happen"
    | VF_exists { left; rest } ->
        unify_check ~at ~hole left;
        unify_check_closure ~at ~hole rest
    | VI_pair { left; rest } ->
        unify_check ~at ~hole left;
        unify_check ~at ~hole rest
    | VE_fst { pair } -> unify_check ~at ~hole pair
    | VE_snd { pair } -> unify_check ~at ~hole pair
    | VF_empty -> failwith "unify_check: VF_empty should never happen"
    | VI_empty -> failwith "unify_check: VI_empty should never happen"
    | VF_record { label = _; left; rest } ->
        unify_check ~at ~hole left;
        unify_check_closure ~at ~hole rest
    | VI_record { label = _; left; rest } ->
        unify_check ~at ~hole left;
        unify_check ~at ~hole rest
    | VE_field { record; label = _ } -> unify_check ~at ~hole record
    | VF_never -> failwith "unify_check: VF_never should never happen"
    | VF_enum { label = _; left; rest } ->
        unify_check ~at ~hole left;
        unify_check ~at ~hole rest
    | VI_enum { label = _; content } -> unify_check ~at ~hole content
    | VE_match { pred; cases } ->
        unify_check ~at ~hole pred;
        unify_check_cases ~at ~hole cases

  and unify_check_env ~at ~hole env =
    (* TODO: short circuit by having intermediary levels on the env *)
    e_iter env (fun _var value -> unify_check ~at ~hole value)

  and unify_check_closure ~at ~hole closure =
    (* TODO: eval vs going through the env? *)
    (* TODO: check the level of the env to shortcircuit *)
    let (Closure (env, _bound, _term)) = closure in
    unify_check_env ~at ~hole env

  and unify_check_cases ~at ~hole cases =
    let (Cases (env, _cases)) = cases in
    unify_check_env ~at ~hole env

  let unify_check ~hole to_ =
    let at = v_level hole in
    unify_check ~at ~hole to_

  let rec split_vf_record ~at env acc rows =
    match v_desc rows with
    | V_hole | VF_empty -> (acc, rows)
    | VF_record { label; left; rest } ->
        (* TODO: this seems like it will be slow *)
        let skolem : value = _ in
        let at = Level.next at in
        split_vf_record ~at env ((label, left, skolem) :: acc) rest
    | _ -> failwith "split_vf_record: not a record"

  let rec split_rows acc rows =
    match v_desc rows with
    | V_hole | VF_unit | VF_empty | VF_never -> (acc, rows)
    | VF_record { label; left; rest } ->
        (* TODO: this will be slow *)
        let skolem : value = _ in
        split_vf_record ((label, left) :: acc) rest
    | _ -> failwith "split_vf_record: not a record"

  (* {vf_vi}_  *)
  let split_rows : value -> (label * value * value) list * value = _
  let split_vf_enum : value -> (label * value * value) list * value = _

  let equal_label received expected =
    (* TODO: physical identity *)
    (* TODO: link if true *)
    let (Label { content = received; loc = _ }) = received in
    let (Label { content = expected; loc = _ }) = expected in
    String.equal received expected

  let unify_label received expected =
    (* TODO: link *)
    match equal_label received expected with
    | true -> ()
    | false -> failwith "unify_label: label clash"

  let rec unify received expected =
    match v_same received expected with
    | true -> ()
    | false ->
        (* TODO: link reason *)
        set_link received ~to_:expected;
        unify_desc received expected

  and unify_desc received expected =
    match (v_desc received, v_desc expected) with
    (* core *)
    | V_hole, _ -> unify_check ~hole:received expected
    | _, V_hole -> unify_check ~hole:expected received
    (* box *)
    | VF_box { type_ = received }, VF_box { type_ = expected } ->
        unify received expected
    | VI_box { content = received }, VI_box { content = expected } ->
        unify received expected
    | VE_unbox { box = received }, VE_unbox { box = expected } ->
        unify received expected
    (* functions *)
    | ( VF_forall { param = received_param; body = received_body },
        VF_forall { param = expected_param; body = expected_body } ) ->
        (* TODO: contravariance? *)
        unify received_param expected_param;
        unify_closure received_body expected_body
    | VI_lambda { body = received_body }, VI_lambda { body = expected_body } ->
        unify_closure received_body expected_body
    | ( VE_apply { funct = received_funct; arg = received_arg },
        VE_apply { funct = expected_funct; arg = expected_arg } ) ->
        unify received_funct expected_funct;
        unify received_arg expected_arg
    (* pairs *)
    | ( VF_exists { left = received_left; rest = received_rows },
        VF_exists { left = expected_left; rest = expected_rows } ) ->
        unify received_left expected_left;
        unify_closure received_rows expected_rows
    | ( VI_pair { left = received_left; rest = received_rows },
        VI_pair { left = expected_left; rest = expected_rows } ) ->
        unify received_left expected_left;
        unify received_rows expected_rows
    | VE_fst { pair = received_pair }, VE_fst { pair = expected_pair } ->
        unify received_pair expected_pair
    | VE_snd { pair = received_pair }, VE_snd { pair = expected_pair } ->
        unify received_pair expected_pair
    (* records *)
    | ( VF_record
          { label = received_label; left = received_left; rest = received_rows },
        VF_record
          { label = expected_label; left = expected_left; rest = expected_rows }
      ) ->
        _
    | ( VI_record
          { label = received_label; left = received_left; rest = received_rows },
        VI_record
          { label = expected_label; left = expected_left; rest = expected_rows }
      ) ->
        _
    | ( VE_field { record = received_record; label = received_label },
        VE_field { record = expected_record; label = expected_label } ) ->
        unify received_record expected_record;
        unify_label received_label expected_label
    (* enum *)
    | ( VF_enum
          { label = received_label; left = received_left; rest = received_rows },
        VF_enum
          { label = expected_label; left = expected_left; rest = expected_rows }
      ) ->
        _
    | ( VI_enum { label = received_label; content = received_content },
        VI_enum { label = expected_label; content = expected_content } ) ->
        unify_label received_label expected_label;
        unify received_content expected_content
    | ( VE_match { pred = received_pred; cases = received_cases },
        VE_match { pred = expected_pred; cases = expected_cases } ) ->
        unify received_pred expected_pred;
        unify received_cases expected_cases
    | _, _ -> _

  and unify_record received expected = v_stru
  and unify_closure received expected = _

  let split_vf_box : value -> value = _
  let split_vf_exists : value -> value * closure = _
  let split_vf_forall : value -> value * closure = _
end

module Typer = struct
  open Core
  open Value
  open Machinery

  type context

  let with_subst : context -> pat -> value -> context = _
  let with_skolem : context -> pat -> value -> context = _
  let v_hole : context -> value = _
  let v_thunk : context -> term -> value = _
  let eval_fst : value -> value = _
  let eval_closure : value -> closure -> value = _
  let subtype : received:value -> expected:value -> unit = _
  let inst : context -> Var.t -> expected:value -> unit = _
  let split_v_sort : value -> value = _

  let rec infer_term ctx term =
    let expected = v_hole ctx in
    check_term ctx term expected;
    expected

  and check_term ctx term expected =
    let (Term { desc; loc }) = term in
    match desc with
    | T_type -> unify vf_type expected
    | T_data -> unify vf_type expected
    | T_annot { term; annot } ->
        let annot = check_annot ctx annot in
        subtype ~received:annot ~expected;
        check_term ctx term annot
    | T_let { bound; arg; body } ->
        let arg_type = infer_pat ~loc ctx bound in
        check_term ctx arg arg_type;
        let arg = v_thunk ctx arg in
        let env = with_subst ctx bound arg in
        check_term env body expected
    | T_var { var } -> inst ctx var ~expected
    | T_hole _ -> _
    | TF_box { type_ } ->
        let sort = split_v_sort expected in
        check_term ctx type_ sort
    | TI_box { content } ->
        let content_type = split_vf_box expected in
        check_term ctx content content_type
    | TE_unbox { box } ->
        (* TODO:  *)
        let box_type = vf_box expected in
        check_term ctx box box_type
    | TF_nominal { type_ } -> _
    | TI_nominal { content } -> _
    | TF_forall { bound; param; body } ->
        let sort = split_v_sort expected in
        let param = check_annot ~loc ctx param in
        check_pat ~loc ctx bound param;
        let env = with_skolem ctx bound param in
        check_term env body sort
    | TI_lambda { bound; body } ->
        let param_type, body_type = split_vf_forall expected in
        let ctx = with_skolem ctx bound param_type in
        let body_type = _ in
        check_term ctx body body_type
    | TE_apply { funct; arg } ->
        let forall = infer_term ctx funct in
        let param_type, body_type = split_vf_forall forall in
        check_term ctx arg param_type;
        let arg = v_thunk ctx arg in
        let body_type = eval_closure arg body_type in
        subtype ~received:body_type ~expected
    | TF_unit -> unify vf_data expected
    | TI_unit -> unify vf_unit expected
    | TF_exists { bound; left; rest } ->
        let sort = split_v_sort expected in
        let left_type = check_annot ~loc ctx left ~sort in
        check_pat ~loc ctx bound left_type;
        let sort = split_v_sort expected in
        let left_type = infer_pat ~loc ctx bound in
        check_term ctx left left_type;
        let left = v_thunk ctx left in
        let env = with_subst ctx bound left in
        check_term env rest _
    | TI_pair { left; rest } ->
        let left_type, rest_type = split_vf_exists expected in
        check_term ctx left left_type;
        (* TODO: enforce rest_type kind *)
        let left = v_thunk ctx left in
        let rest_type = eval_closure left rest_type in
        check_term ctx rest rest_type
    | TE_fst { pair } ->
        let pair_type = infer_term ctx pair in
        let fst_type, _snd_type = split_vf_exists pair_type in
        unify fst_type expected
    | TE_snd { pair } ->
        let pair_type = infer_term ctx pair in
        let _fst_type, snd_type = split_vf_exists pair_type in
        (* TODO: no intermediary thunk? *)
        let fst = eval_fst @@ v_thunk ctx pair in
        let snd_type = eval_closure fst snd_type in
        unify snd_type expected
    | TF_empty -> unify vf_data expected
    | TI_empty -> unify vf_empty expected
    | TF_record { label; left; rest } -> _
    | TI_record { label; left; rest } ->
        (* TODO: enforce rest kind *)
        _
    | TE_field { record; label } -> _
    | TF_never -> unify vf_data expected
    | TF_enum { label; left; rest } ->
        let sort = split_v_sort expected in
        _
    | TI_enum { label; content } -> _
    | TE_match _ -> _

  and check_annot ~loc env annot ~sort = _

  and infer_pat ~loc env pat =
    match pat with
    | P_loc { pat; loc } -> infer_pat ~loc env pat
    | P_annot { pat; annot } ->
        let annot = check_annot ~loc env annot in
        check_pat ~loc env pat annot;
        annot
    | P_var _ | P_alias _ | P_or _ | P_box _ | P_unit | P_pair _ | P_empty
    | P_record _ | P_enum _ ->
        _

  and check_pat ~loc env pat expected =
    match pat with
    | P_loc { pat; loc } -> check_pat ~loc env pat expected
    | P_annot _ | P_var _ | P_alias _ | P_or _ | P_box _ | P_unit | P_pair _
    | P_empty | P_record _ | P_enum _ ->
        _
end

(* TODO: this abbreviation doesn't have a good reason
  just me being annoying *)
(* TODO: better docs *)
(* boxed nominal value *)

(* TODO: rename to value and term? *)
(* TODO: document how this works *)
(* TODO: id for tooling reasons? *)
(* TODO: hash consing *)
(* TODO: unit, empty, never could be var *)
(* TODO: link reason *)
