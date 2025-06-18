open Teejparser
open Sedlexing.Utf8

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\n')]
let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]

(* TODO: escape, proper strings *)

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | number ->
      let content = lexeme buf in
      NUMBER (int_of_string content)
  | "+" -> PLUS
  | "-" -> DASH
  | "*" -> ASTERISK
  | "/" -> BAR
  | "(" -> LEFT_PARENS
  | ")" -> RIGHT_PARENS
  | eof -> EOF
  | _ -> failwith "unknown token"

let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  (token, start, stop)

let from_string parser string =
  let buf = from_string string in
  let provider = provider buf in
  MenhirLib.Convert.Simplified.traditional2revised parser provider

let print code =
  let expr = Option.get @@ from_string Teejparser.expr_opt code in
  Format.eprintf "%a\n%!" Tree.pp_expr expr

let () = print "1 + 2 * 3 + 4"
