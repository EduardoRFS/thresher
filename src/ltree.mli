type expr =
  (* x *)
  | LE_var of { var : string }
  (* P => m *)
  | LE_lambda of { param : pat; return : expr }
  (* (m n) *)
  | LE_apply of { lambda : expr; arg : expr }
  (* let x = m in n *)
  | LE_let of { bound : pat; value : expr; return : expr }
  (* (m : T) *)
  | LE_annot of { expr : expr; annot : annot }

and annot =
  (* X *)
  | LA_var of { var : string }
  (* A -> B *)
  | LA_arrow of { param : annot; return : annot }

and pat =
  (* x *)
  | LP_var of { var : string }
  (* x : T *)
  | LP_annot of { pat : pat; annot : annot }
[@@deriving show]
