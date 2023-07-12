open Ltree
open Ttree
open Context
open Machinery

(* TODO: lookup expected??? *)
let lookup_type ctx ~var ~expected =
  match lookup_type ctx ~var with
  | Some (var, type_) ->
      (* TODO: is this needed? *)
      let type_ = instance ctx type_ in
      let () = subtype ~received:type_ ~expected in
      Some var
  | None -> None

let lookup_value ctx ~var ~expected =
  match lookup_value ctx ~var with
  | Some (var, type_) ->
      (* TODO: is this needed? yes, because of mutation *)
      let type_ = instance ctx type_ in
      let () = subtype ~received:type_ ~expected in
      Some var
  | None -> None

let rec check_expr ctx expr ~expected =
  let desc = check_expr_desc ctx expr ~expected in
  TE { desc; type_ = expected }

and check_expr_desc ctx expr ~expected =
  match expr with
  | LE_var { var } -> (
      match lookup_value ctx ~var ~expected with
      | Some var -> TE_var { var }
      | None -> failwith @@ Format.sprintf "unknown variable: %s" var)
  | LE_lambda { param; return } ->
      let expected_param, expected_return = split_arrow ctx expected in
      check_pat ctx param ~expected:expected_param @@ fun param ->
      let return = check_expr ctx return ~expected:expected_return in
      TE_lambda { param; return }
  | LE_apply { lambda; arg } ->
      let param = new_hole ctx in
      let lambda =
        let expected = T_arrow { param; return = expected } in
        check_expr ctx lambda ~expected
      in
      let arg = check_expr ctx arg ~expected:param in
      TE_apply { lambda; arg }
  | LE_let { bound; value; return } ->
      (* TODO: better way to do generalization than this *)
      let () = enter_level ctx in
      let value_expected = new_hole ctx in
      let value = check_expr ctx value ~expected:value_expected in
      check_pat ctx bound ~expected:value_expected @@ fun bound ->
      let () = leave_level ctx in
      generalize ctx value_expected;

      let return = check_expr ctx return ~expected in
      TE_let { bound; value; return }
  | LE_annot { expr; annot } ->
      let annot = check_annot ctx annot ~expected in
      let expr = check_expr ctx expr ~expected in
      TE_annot { expr; annot }

and check_annot ctx annot ~expected =
  let desc = check_annot_desc ctx annot ~expected in
  TA { desc; type_ = expected }

and check_annot_desc ctx annot ~expected =
  match annot with
  | LA_var { var } -> (
      match lookup_type ctx ~var ~expected with
      | Some var -> TA_var { var }
      | None -> failwith @@ Format.sprintf "unknown type: %s" var)
  | LA_arrow { param; return } ->
      let expected_param, expected_return = split_arrow ctx expected in
      let param = check_annot ctx param ~expected:expected_param in
      let return = check_annot ctx return ~expected:expected_return in
      TA_arrow { param; return }

and check_pat ctx pat ~expected k =
  check_pat_desc ctx pat ~expected @@ fun desc ->
  k @@ TP { desc; type_ = expected }

and check_pat_desc ctx pat ~expected k =
  match pat with
  | LP_var { var } ->
      with_value ctx ~var expected @@ fun var -> k @@ TP_var { var }
  | LP_annot { pat; annot } ->
      (* TODO: direction during pattern annotation *)
      let annot = check_annot ctx annot ~expected in
      check_pat ctx pat ~expected @@ fun pat -> k @@ TP_annot { pat; annot }

let infer_expr ctx expr =
  let expected = new_hole ctx in
  check_expr ctx expr ~expected
