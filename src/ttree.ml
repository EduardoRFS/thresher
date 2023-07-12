assert (Sys.int_size = 63)

type type_ =
  | T_constr of { level : int; var : Var.t }
  (* A -> B *)
  | T_arrow of { param : type_; return : type_ }
  (* X | x *)
  | T_var of { mutable level : int; mutable link : type_ }
[@@deriving show { with_path = false }]

(* core language *)
type expr = TE of { desc : expr_desc; type_ : type_ }

and expr_desc =
  | TE_var of { var : Var.t }
  | TE_lambda of { param : pat; return : expr }
  | TE_apply of { lambda : expr; arg : expr }
  | TE_let of { bound : pat; value : expr; return : expr }
  | TE_annot of { expr : expr; annot : annot }

(* type annotation *)
and annot = TA of { desc : annot_desc; type_ : type_ }

and annot_desc =
  | TA_var of { var : Var.t }
  | TA_arrow of { param : annot; return : annot }

(* pattern language *)
and pat = TP of { desc : pat_desc; type_ : type_ }

and pat_desc =
  | TP_var of { var : Var.t }
  | TP_annot of { pat : pat; annot : annot }
[@@deriving show { with_path = false }]
