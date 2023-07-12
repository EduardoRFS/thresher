type expr =
  (* TODO: count occurences during language transformation
      to use during instantiation *)
  | LE_var of { var : string }
  | LE_lambda of { param : pat; return : expr }
  | LE_apply of { lambda : expr; arg : expr }
  | LE_let of { bound : pat; value : expr; return : expr }
  (* TODO: expr is a bad name *)
  | LE_annot of { expr : expr; annot : annot }

and annot =
  (* X *)
  | LA_var of { var : string }
  (* A -> B *)
  | LA_arrow of { param : annot; return : annot }

and pat =
  | LP_var of { var : string }
  | LP_annot of { pat : pat; annot : annot }
[@@deriving show]
