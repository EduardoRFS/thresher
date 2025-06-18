module M = struct
  (* NbE + BiDi aka normalization by evaluation *)
  type var = string
  type level = int

  (* terms, the core of the whole thing, checked and then computed on *)
  type term =
    (* x *)
    | T_var of var
    (* x = M; N *)
    | T_let of var * term * term
    (* (x : A) -> B *)
    | T_forall of var * term * term
    (* M(N) *)
    | T_apply of term * term
    (* (x) => M *)
    | T_lambda of var * term

  (* values, produced from terms, can be reified back to terms *)
  (* Main difference from something like OCaml is that
  the values are not only types, but also functions and other values *)
  (*
  Every value is bound to a let and reference to a let

  (x => M)(N)

  in values is 

  f = M;
  x = N;
  f(x)
*)
  type value = {
    mutable value_desc : value_desc;
    mutable value_level : level;
    mutable value_link : value option;
  }

  and value_desc =
    | V_forall of var * value * value
    | V_lambda of var * value
    (* open / neutral terms *)
    | VO_var of var
    | VO_apply of value * value

  (* the environment, a list of bindings *)
  and env =
    (* _ *)
    | L_hole
    (* x = M; L *)
    | L_let of var * value * env

  and closure =
    (* L<x = _; M> *)
    | Closure of env * var * term

  (* L<M> |-> N *)
  let rec eval env term =
    match term with
    | T_var _ -> _
    | T_let (var, arg, body) ->
        let arg = eval env arg in
        let env = L_let (var, arg, env) in
        eval env body
    | T_forall (_, _, _) -> _
    | T_apply (_, _) -> _
    | T_lambda (_, _) -> _
  (* the environment, a list of bindings *)
end
