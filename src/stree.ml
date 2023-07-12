type term =
  | ST_parens of { term : term }
  | ST_var of { var : string }
  | ST_forall of { var : term; return : term }
  | ST_arrow of { param : term; return : term }
  | ST_lambda of { param : term; return : term }
  | ST_apply of { lambda : term; arg : term }
  | ST_let of { bound : term; value : term; return : term }
  | ST_annot of { term : term; annot : term }
[@@deriving show { with_path = false }]
