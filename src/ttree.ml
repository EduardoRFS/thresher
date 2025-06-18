assert (Sys.int_size = 63)

(* TODO: IR format choosen to make it similar enough to Grain and OCaml *)

(* TODO: location stack *)

(* TODO: different tree for LSP, allows to reconstruct data? *)

(* TODO: use unified tree on ttree *)
open Types

type kind =
  | K_any
  (* boxed value *)
  | K_val
  (* unboxed value*)
  | K_raw
[@@deriving show { with_path = false }]

type error = EA_unknown_var of { var : Var.t } | EA_arith_clash
[@@deriving show { with_path = false }]

type 'a fields =
  | Fields of {
      desc : 'a fields_desc;
      infer : type_;
      check : type_;
      loc : Location.t; [@opaque]
    }

and 'a fields_desc = F_pair of { fst : 'a; snd : 'a fields } | F_null
[@@deriving show { with_path = false }]

type annot =
  | TAnnot of {
      desc : annot_desc;
      type_ : type_;
      loc : Location.t; [@opaque]
      errors : error list ref;
    }

and annot_desc =
  | TA_constr of { var : Var.t; args : annot list }
  | TA_arrow of { params : annot list; return : annot }
  | TA_unit
  | TA_tuple of { fields : annot fields }
[@@deriving show { with_path = false }]

type pat =
  | TPat of {
      desc : pat_desc;
      infer : type_;
      check : type_;
      loc : Location.t; [@opaque]
      errors : error list ref;
    }

and pat_desc =
  | TP_annot of { pat : pat; annot : annot }
  | TP_alias of { pat : pat; as_ : Var.t }
  | TP_var of { var : Var.t }
  (* TODO: rows vs rest *)
  | TP_box of { pat : pat }
  | TP_unit
  | TP_pair of { left : pat; rest : pat }
  | TP_empty
  | TP_record of { label : string; left : pat; rest : pat }
[@@deriving show { with_path = false }]

type expr =
  | TExpr of {
      desc : expr_desc;
      type_ : type_;
      loc : Location.t; [@opaque]
      errors : error list ref;
    }

and expr_desc =
  | TE_annot of { expr : expr; annot : annot }
  | TE_var of { var : Var.t }
  | TE_lambda of { param : pat; body : expr }
  | TE_apply of { lambda : expr; arg : expr }
  | TE_box of { value : expr }
  | TE_unit
  | TE_pair of { left : expr; rest : expr }
  | TE_empty
  | TE_record of { label : string; left : expr; rest : expr }
  (* TODO: payload vs arg vs row *)
  | TE_enum of { label : string; payload : expr }
  | TE_check of { expr : expr }
  | TE_block of { block : block }

and block =
  | TBlock of {
      desc : block_desc;
      loc : Location.t; [@opaque]
      errors : error list ref;
    }

(* TODO: rename return to body? *)
(* TODO: early return *)
and block_desc =
  (* TODO: recursive and mutable flags *)
  | TB_let of { bound : pat; value : expr; return : block }
  | TB_eval of { value : expr; return : block }
  | TB_return of { return : expr }
[@@deriving show { with_path = false }]

(* TODO: errors on rows? *)
type record_row =
  | TRRow of { name : string; payload : annot; loc : Location.t [@opaque] }
[@@deriving show { with_path = false }]

type enum_row_payload =
  | TERP_tuple of { fields : annot list; loc : Location.t [@opaque] }
  | TERP_record of { rows : record_row list; loc : Location.t [@opaque] }
  | TERP_none
[@@deriving show { with_path = false }]

type enum_row =
  | TERow of {
      name : string;
      payload : enum_row_payload;
      loc : Location.t; [@opaque]
    }
[@@deriving show { with_path = false }]

type type_binding =
  | TTBind of {
      var : Var.t;
      params : Var.t list;
      desc : type_binding_desc;
      constr : constr;
      loc : Location.t; [@opaque]
      mutable errors : error list;
    }

and type_binding_desc =
  | TTB_alias of { body : annot }
  | TTB_record of { rows : record_row list }
  | TTB_enum of { rows : enum_row list }
[@@deriving show { with_path = false }]

(* helpers *)
let nil_level = 0
let tt_nil : type_ = assert false
let t_unit : type_ = assert false
let is_tt_nil type_ = type_ == tt_nil
