open Types
open Ttree

module Names = struct
  type names
  type t = names
end

module Vars = struct
  type vars
  type t = vars
end

(* TODO: put this on Ttree? *)
let type_of_tannot tannot =
  let (TAnnot { desc = _; type_; loc = _; errors = _ }) = tannot in
  type_

let type_of_tpat tpat =
  let (TPat { desc = _; type_; loc = _; errors = _ }) = tpat in
  type_

let type_of_texpr texpr =
  let (TExpr { desc = _; type_; loc = _; errors = _ }) = texpr in
  type_

let rec type_of_tblock tblock =
  (* TODO: should this be O(1)? *)
  let (TBlock { desc = block; loc = _; errors = _ }) = tblock in
  match block with
  | TB_let { bound = _; value = _; return } -> type_of_tblock return
  | TB_eval { value = _; return } -> type_of_tblock return
  | TB_return { return } -> type_of_texpr return

module Elaborate = struct
  open Ltree
  open Ttree

  module rec Context : sig
    type context
    type t = context

    val new_hole : context -> type_

    (* TODO: loc for value and type_ *)
    val enter_value_var :
      context -> var:Var.t -> type_:type_ -> (unit -> 'a) -> 'a

    val solve_value_var : context -> var:string -> Var.t
    val enter_constr : context -> var:string -> (unit -> 'a) -> 'a
    val solve_constr : context -> var:string -> Var.t

    (* errors *)
    val fork : context -> (unit -> 'a) -> 'a
    val error : context -> error -> unit
    val dump : context -> error list

    (* TODO: the following functions may raise errors *)
    val new_hole_bound_by_record :
      context -> closed:bool -> rows:(string * type_) list -> type_

    val new_constr : context -> constr:constr -> args:type_ list -> type_
  end =
    Context
  (* struct
       module String_tbl = Hashtbl.Make (String)

       type context = { mutable names : Var.t String_tbl.t }
       type t = context
     end *)

  open Context

  (* TODO: document, elaborate never inspect types *)
  let rec enter_pat ctx pat k =
    let (TPat { desc; type_; loc = _; errors = _ }) = pat in
    match desc with
    | TP_annot { pat; annot = _ } -> enter_pat ctx pat k
    | TP_alias { pat; as_ } ->
        enter_value_var ctx ~var:as_ ~type_ @@ fun () -> enter_pat ctx pat k
    | TP_var { var } -> enter_value_var ctx ~var ~type_ k
    | TP_tuple { fields } -> enter_many_pats ctx fields k

  and enter_many_pats ctx pats k =
    match pats with
    | [] -> k ()
    | pat :: pats -> enter_pat ctx pat @@ fun () -> enter_many_pats ctx pats k

  let rec solve_annot ctx annot =
    let (LAnnot { desc = annot; loc }) = annot in
    let tannot desc =
      let type_ = new_hole ctx in
      let errors = dump ctx in
      TAnnot { desc; type_; loc; errors }
    in
    fork ctx @@ fun () ->
    match annot with
    | LA_constr { var; args } ->
        let args = List.map (fun arg -> solve_annot ctx arg) args in
        let var = solve_constr ctx ~var in
        tannot @@ TA_constr { var; args }
    | LA_arrow { params; return } ->
        let params = List.map (fun param -> solve_annot ctx param) params in
        let return = solve_annot ctx return in
        tannot @@ TA_arrow { params; return }
    | LA_tuple { fields } ->
        let fields = List.map (fun field -> solve_annot ctx field) fields in
        tannot @@ TA_tuple { fields }

  (* TODO: maybe store also expected type? *)
  let rec solve_pat ctx pat =
    let (LPat { desc = pat; loc }) = pat in
    let tpat desc =
      let type_ = new_hole ctx in
      let errors = dump ctx in
      TPat { desc; type_; loc; errors }
    in
    fork ctx @@ fun () ->
    match pat with
    | LP_annot { pat; annot } ->
        let annot = solve_annot ctx annot in
        let pat = solve_pat ctx pat in
        tpat @@ TP_annot { pat; annot }
    | LP_alias { pat; as_ } ->
        let pat = solve_pat ctx pat in
        let as_ = Var.create as_ in
        tpat @@ TP_alias { pat; as_ }
    | LP_var { var } ->
        let var = Var.create var in
        tpat @@ TP_var { var }
    | LP_tuple { fields } ->
        let fields = List.map (fun field -> solve_pat ctx field) fields in
        tpat @@ TP_tuple { fields }
    | LP_record { rows } ->
        (* TODO: uniqueness of names *)
        let rows =
          List.map
            (fun (name, payload) ->
              let payload = solve_pat ctx payload in
              (name, payload))
            rows
        in
        tpat @@ TP_record { rows }

  let rec solve_expr ctx expr =
    let (LExpr { desc = expr; loc }) = expr in
    let texpr desc =
      let type_ = new_hole ctx in
      let errors = dump ctx in
      TExpr { desc; type_; loc; errors }
    in
    fork ctx @@ fun () ->
    match expr with
    | LE_annot { expr; annot } ->
        let annot = solve_annot ctx annot in
        let expr = solve_expr ctx expr in
        texpr @@ TE_annot { expr; annot }
    | LE_var { var } ->
        let var = solve_value_var ctx ~var in
        texpr @@ TE_var { var }
    | LE_lambda { params; return } ->
        let inner_ctx, params =
          List.fold_left_map
            (fun inner_ctx param -> solve_pat inner_ctx param)
            ctx params
        in
        let return = solve_expr inner_ctx return in
        texpr @@ TE_lambda { params; return }
    | LE_apply { lambda; args } ->
        let lambda = solve_expr ctx lambda in
        let args = List.map (fun arg -> solve_expr ctx arg) args in
        texpr @@ TE_apply { lambda; args }
    | LE_tuple { fields } ->
        let fields = List.map (fun field -> solve_expr ctx field) fields in
        texpr @@ TE_tuple { fields }
    | LE_record { rows } ->
        let rows =
          List.map
            (fun (name, payload) ->
              let payload = solve_expr ctx payload in
              (name, payload))
            rows
        in
        texpr @@ TE_record { rows }
    | LE_block { block } ->
        let block = solve_block ctx block in
        texpr @@ TE_block { block }

  and solve_block ctx block =
    let (LBlock { desc = block; loc }) = block in
    let tblock desc =
      let errors = dump ctx in
      TBlock { desc; loc; errors }
    in
    fork ctx @@ fun () ->
    match block with
    | LB_let { bound; value; return } ->
        let inner_ctx, bound = solve_pat ctx bound in
        let value =
          (* TODO: recursive *)
          (* TODO: always adds but tag as not valid *)
          solve_expr ctx value
        in
        let return = solve_block ctx return in
        tblock @@ TB_let { bound; value; return }
    | LB_eval { value; return } ->
        let value = solve_expr ctx value in
        let return = solve_block ctx return in
        tblock @@ TB_eval { value; return }
    | LB_return { return } ->
        let return = solve_expr ctx return in
        tblock @@ TB_return { return }

  let rec solve_type_binding ctx binding =
    let (LTBind { var; params; desc; loc }) = binding in
    let var = Var.create var in
    let params = List.map (fun param -> Var.create param) params in
    let ttbind constr desc =
      let errors = dump ctx in
      TTBind { var; params; desc; constr; loc; errors }
    in
    match desc with
    | LTB_alias { body } ->
        let body = solve_annot ctx body in
        let constr =
          let body = type_of_tannot body in
          new_alias ~body
        in
        ttbind constr @@ TTB_alias { body }
    | LTB_record { rows } ->
        (* TODO: ensure no duplicated row *)
        let rows =
          List.map
            (fun row ->
              let (LRRow { name; payload; loc }) = row in
              let payload = solve_annot ctx payload in
              TRRow { name; payload; loc })
            rows
        in
        ttbind @@ TTB_record { rows }
    | LTB_enum _ ->
        (* TODO: ensure no duplicated row *)
        failwith "not implemented"
end

module Infer_and_propagate = struct
  open Ttree

  module rec Context : sig
    type context
    type t = context

    (* errors *)
    val load : context -> errors:error list ref -> (unit -> 'a) -> 'a
    val error : context -> error -> unit
  end =
    Context

  open Context

  let rec check_annot ctx annot ~expected =
    let (TAnnot { desc = annot; type_ = received; loc = _; errors }) = annot in
    load ctx ~errors @@ fun () ->
    (* TODO: this could go on the error reporting side *)
    match annot with
    | TA_constr { var = _; args } ->
        let expected_args = unify_head_constr ~received ~expected in
        (* TODO: arith clash *)
        List.iter2
          (fun arg expected_arg -> check_annot ctx arg ~expected:expected_arg)
          args expected_args
    | TA_arrow { params; return } ->
        let expected_params, expected_return =
          unify_head_arrow ~received ~expected
        in
        (* TODO: arith clash *)
        List.iter2
          (fun param expected_param ->
            check_annot ctx param ~expected:expected_param)
          params expected_params;
        check_annot ctx return ~expected:expected_return
    | TA_tuple { fields } ->
        let expected_fields = unify_head_tuple ~received ~expected in
        (* TODO: arith clash *)
        List.iter2
          (fun field expected_field ->
            check_annot ctx field ~expected:expected_field)
          fields expected_fields

  let v_last _ : type_ = _
  let v_arrow () : type_ = _
  let v_pair () : type_ = _

  let rec infer_pat ctx pat =
    let expected = type_of_tpat pat in
    check_pat ctx pat ~expected;
    expected

  and check_pat ctx pat expected : unit =
    let (TPat { desc; infer = _; check = received; loc = _; errors }) = pat in
    load ctx ~errors @@ fun () ->
    match (desc : pat_desc) with
    | TP_annot { pat; annot } ->
        check_annot ctx annot ~expected;
        check_pat ctx pat (type_of_tannot annot)
    | TP_var { var = _ } -> unify ctx ~received ~expected
    | TP_alias { pat; as_ = _ } -> check_pat ctx pat expected
    | TP_unit -> _
    | TP_pair { fst; snd; snd_kind } ->
        let pair, fst_type, snd_type = v_pair ~snd_kind () in
        write pair;
        check_pat ctx fst fst_type;
        check_pat ctx snd snd_type
    | TP_record { rows } ->
        (* TODO: open *)
        assert false

  and check_pat_tuple ctx fields expected =
    let (Fields { desc; infer; check; loc }) = fields in
    match desc with
    | F_null ->
        (* TODO: empty pair? *)
        _
    | [ fst ] ->
        let expected = v_return expected in
        check_pat ctx fst expected
    | fst :: snd ->
        let fst_type, snd_type = v_pair _ in
        check_pat ctx fst fst_type;
        check_pat_tuple ctx snd snd_type

  let rec infer_expr ctx expr =
    let expected = type_of_texpr expr in
    check_expr ctx expr ~expected;
    expected

  and check_expr ctx expr expected =
    let (TExpr { desc; infer = _; check = _; loc = _; errors }) = expr in
    (* TODO: context is only for levels? *)
    load ctx ~errors @@ fun () ->
    match desc with
    | TE_annot { expr; annot } ->
        check_annot ctx annot ~expected;
        check_expr ctx expr ~expected:(type_of_tannot annot)
    | TE_lambda { params; return } ->
        let param, return = new_arrow () in
        (* TODO: arith clash *)
        List.iter2
          (fun param expected_param ->
            check_pat ctx param ~expected:expected_param)
          params expected_params;
        check_expr ctx return ~expected:expected_return
    (* infer *)
    | TE_var { var = _ } -> unify ctx ~received ~expected
    | TE_apply { lambda; args } ->
        (* TODO: propagate return and args? *)
        let arrow = infer_expr ctx lambda in
        let params, received = split_arrow ctx ~received:arrow in
        unify ctx ~received ~expected;
        (* TODO: arith clash *)
        List.iter2
          (fun arg expected -> check_expr ctx arg ~expected)
          args params
    | TE_tuple { fields } ->
        let expected_fields = unify_head_tuple ~received ~expected in
        (* TODO: arith clash *)
        List.iter2
          (fun field expected_field ->
            check_expr ctx field ~expected:expected_field)
          fields expected_fields
    | TE_record { rows } ->
        (* TODO: closed *)
        assert false
    | TE_block { block } -> check_block ctx block ~expected

  and check_expr_lambda ctx params return expected =
    match params with
    | [] ->
        let expected = v_return expected in
        check_expr ctx return expected
    | param :: params ->
        let param_type, return_type = v_arrow () in
        check_pat ctx param param_type;
        check_expr_lambda ctx params return return_type

  and check_block ctx block ~expected =
    let (TBlock { desc = block; loc = _; errors = _ }) = block in
    match block with
    | TB_let { bound; value; return } ->
        let value_expected = infer_pat ctx bound in
        check_expr ctx value ~expected:value_expected;
        check_block ctx return ~expected
    | TB_eval { value; return } ->
        check_expr ctx value ~expected:t_unit;
        check_block ctx return ~expected
    | TB_return { return } -> check_expr ctx return ~expected
end
