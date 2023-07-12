open Grainhack

type test = { name : string; term : string }

let type_term name term = { name; term }
let id = type_term "id" {|(x : Int) => x|}
let sequence = type_term "sequence" {|x => y => y|}

let let_poly =
  type_term "let poly"
    {|
      let sequence = x => y => y;
      sequence sequence
    |}

let int_annot = type_term "int annot" {|(x : Int) => x|}
let tests = [ id; sequence; let_poly; int_annot ]

let type_term term =
  let term = Slexer.from_string Sparser.term_opt term in
  let term = Option.get term in
  let expr = Lparser.parse_expr term in
  let open Context in
  let open Ttyper in
  let ctx = make () in
  with_type ctx ~var:"Int" @@ fun _int ->
  let () = enter_level ctx in
  infer_expr ctx expr

let test { name; term } =
  let check () =
    let expr = type_term term in
    let (TE { desc = _desc; type_ = _type }) = expr in
    (* Format.eprintf "%a\n%!" Tprinter.pp_type _type; *)
    ()
  in
  Alcotest.test_case name `Quick check

let tests = ("tests", List.map test tests)
let () = Alcotest.run "Typer" [ tests ]
