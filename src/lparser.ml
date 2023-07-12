open Stree
open Ltree

(* TODO: print loc and term *)
exception Invalid_notation

let rec parse_expr term =
  match term with
  | ST_parens { term } -> parse_expr term
  | ST_var { var } -> LE_var { var }
  | ST_lambda { param; return } ->
      let param = parse_pat param in
      let return = parse_expr return in
      LE_lambda { param; return }
  | ST_apply { lambda; arg } ->
      let lambda = parse_expr lambda in
      let arg = parse_expr arg in
      LE_apply { lambda; arg }
  | ST_let { bound; value; return } ->
      let bound = parse_pat bound in
      let value = parse_expr value in
      let return = parse_expr return in
      LE_let { bound; value; return }
  | ST_annot { term; annot } ->
      let expr = parse_expr term in
      let annot = parse_annot annot in
      LE_annot { expr; annot }
  | ST_forall _ | ST_arrow _ -> raise Invalid_notation

and parse_annot term =
  match term with
  | ST_parens { term = type_ } -> parse_annot type_
  | ST_var { var } -> LA_var { var }
  | ST_arrow { param; return } ->
      let param = parse_annot param in
      let return = parse_annot return in
      LA_arrow { param; return }
  | ST_forall _ | ST_lambda _ | ST_apply _ | ST_let _ | ST_annot _ ->
      raise Invalid_notation

and parse_pat term =
  match term with
  | ST_parens { term = pat } -> parse_pat pat
  | ST_var { var } -> LP_var { var }
  | ST_annot { term = pat; annot } ->
      let pat = parse_pat pat in
      let annot = parse_annot annot in
      LP_annot { pat; annot }
  | ST_forall _ | ST_arrow _ | ST_lambda _ | ST_apply _ | ST_let _ ->
      raise Invalid_notation
