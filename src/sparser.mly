%{
open Stree
%}
%token <string> VAR (* x *)
%token ARROW (* -> *)
%token LAMBDA (* => *)
%token LET (* let *)
%token EQUAL (* = *)
%token COLON (* : *)
%token SEMICOLON (* ; *)
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)

%token EOF

%start <Stree.term option> term_opt

%%

let term_opt :=
  | EOF;
    { None }
  | term = term; EOF;
    { Some term }

let term := term_rec_annot

let term_rec_annot :=
  | term_rec_let
  | term_annot(term_rec_annot, term_rec_let)

let term_rec_let :=
  | term_rec_funct
  | term_let(term_rec_let, term_rec_funct)

let term_rec_funct :=
  | term_rec_apply
  | term_arrow(term_rec_funct, term_rec_apply)
  | term_lambda(term_rec_funct, term_rec_apply)
 
let term_rec_apply :=
  | term_atom
  | term_apply(term_rec_apply, term_atom)

let term_atom :=
  | term_var
  | term_parens(term)

let term_var ==
  | var = VAR;
    { ST_var { var } }
let term_arrow(self, lower) ==
  | param = lower; ARROW; return = self;
    { ST_arrow { param; return } }
let term_lambda(self, lower) ==
  | param = lower; LAMBDA; return = self;
    { ST_lambda { param; return } }
let term_apply(self, lower) ==
  | lambda = self; arg = lower;
    { ST_apply { lambda; arg } }
let term_let(self, lower) ==
  | LET; bound = lower; EQUAL; value = lower; SEMICOLON; return = self;
    { ST_let { bound; value; return } }
let term_annot(self, lower) ==
  | term = lower; COLON; annot = self;
    { ST_annot { term; annot } }
let term_parens(content) ==
  | LEFT_PARENS; term = content; RIGHT_PARENS;
    { ST_parens { term } }
