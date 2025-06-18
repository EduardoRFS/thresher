type var = { id : int; name : string }
type t = var

let pp fmt var =
  let { name; id } = var in
  Format.fprintf fmt "%s:%d" name id

let show var = Format.asprintf "%a" pp var
let compare l r = Int.compare l.id r.id
let equal l r = Int.equal l.id r.id

let create =
  (* GLOBAL *)
  let next = Atomic.make 0 in
  fun name ->
    let id = Atomic.fetch_and_add next 1 in
    { id; name }

let name var = var.name
