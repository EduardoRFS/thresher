(* TODO: drop all string types *)
type annot = LAnnot of { desc : annot_desc; loc : Location.t [@opaque] }

and annot_desc =
  | LA_constr of { var : string; args : annot list }
  | LA_arrow of { params : annot list; return : annot }
  | LA_tuple of { fields : annot list }
[@@deriving show { with_path = false }]

type pat = LPat of { desc : pat_desc; loc : Location.t [@opaque] }

and pat_desc =
  | LP_annot of { pat : pat; annot : annot }
  (* TODO: alias patterns *)
  | LP_alias of { pat : pat; as_ : string }
  | LP_var of { var : string } 
  | LP_tuple of { fields : pat_fields }
  | LP_record of { rows : (string * pat) list }
and pat_fields =
  | LPFields of {}
and pat_fields_desc =
  | LPF_null
  | LPF_pair of { fst : pat; rest : pat_fields }
[@@deriving show { with_path = false }]

type expr = LExpr of { desc : expr_desc; loc : Location.t [@opaque] }

and expr_desc =
  | LE_annot of { expr : expr; annot : annot }
  | LE_var of { var : string }
  | LE_lambda of { params : pat list; return : expr }
  | LE_apply of { lambda : expr; args : expr list }
  | LE_tuple of { fields : expr list }
  | LE_record of { rows : (string * expr) list }
  | LE_block of { block : block }

and block = LBlock of { desc : block_desc; loc : Location.t [@opaque] }

and block_desc =
  | LB_let of { bound : pat; value : expr; return : block }
  | LB_eval of { value : expr; return : block }
  | LB_return of { return : expr }
[@@deriving show { with_path = false }]

type record_row =
  | LRRow of { name : string; payload : annot; loc : Location.t [@opaque] }
[@@deriving show { with_path = false }]

type enum_row_payload =
  | LERP_tuple of { fields : annot list; loc : Location.t [@opaque] }
  | LERP_record of { rows : record_row list; loc : Location.t [@opaque] }
  | LERP_none
[@@deriving show { with_path = false }]

type enum_row =
  | LERow of {
      name : string;
      payload : enum_row_payload;
      loc : Location.t; [@opaque]
    }
[@@deriving show { with_path = false }]

type type_binding =
  | LTBind of {
      var : string;
      params : string list;
      desc : type_binding_desc;
      loc : Location.t; [@opaque]
    }

and type_binding_desc =
  | LTB_alias of { body : annot }
  | LTB_record of { rows : record_row list }
  | LTB_enum of { rows : enum_row list }
[@@deriving show { with_path = false }]
