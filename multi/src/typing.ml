
type typing_env = (Name.t * (Location.t * Ast.base_type)) list
let empty_env = []
let decl_var_type t env (v, l) =
  if not (List.mem_assoc v env) then
    (v, (l, t))::env
  else
    (failwith ("Variable " ^ v ^ " already declared"))

let get_type loc env n = 
  try
    List.assoc n env
  with Not_found -> 
    (Format.eprintf "Typing error. Could not find declaration for variable %s at location %a@.@?"
       n Location.fprint loc;
     exit 1)

let type_error loc_error expected_t found_t = 
Format.eprintf "Typing error at loc %a. Found type %a, expecting type %a@.@?"
  Location.fprint loc_error
  Ast.pp_base_type found_t
  Ast.pp_base_type expected_t;
  exit 1

(* let type_cst l typ c = *)
(*   if not (typ = Ast.IntT) || (Z.equal (Q.den c) Z.one) then *)
(*     c *)
(*   else *)
(*     type_error l typ Ast.RealT *)
  
let rec type_expr (env:typing_env) typ (ue: Ast.uexpr) =
  let te = type_expr env typ in
  match ue with
  | Ast.UCst (l, (c, t)) -> (
    match t with
      None -> if typ = Ast.IntT && (Z.equal (Q.den c) Z.one) then
	  Ast.mk_expr l typ (Ast.Cst c)
	else 
	  type_error l typ Ast.RealT
    | Some t -> 
      if typ = t then
	Ast.mk_expr l typ (Ast.Cst c)
      else 
	type_error l typ t
  )
  | Ast.UVar (l, n) -> 
    let n_loc, n_t = get_type l env n in
    if n_t = typ then
      Ast.mk_expr l typ (Ast.Var n)
    else
      type_error l typ n_t	

  | Ast.UBinop (l, op, e1, e2) -> 
    Ast.mk_expr l typ (Ast.Binop (op, te e1, te e2))
  | Ast.URand (l, t, c1, c2) -> 
    if typ = t then
      Ast.mk_expr l typ (Ast.Rand (c1, c2))
    else
      type_error l typ t	

let type_guard env (e, cmp) = 
  (* For the moment, force guards to be built over integer.
     TODO: a real typing algorithm
  *)
  type_expr env Ast.IntT e, cmp

let rec type_stm env s = 

  let te loc n = type_expr env (snd (get_type loc env n)) in
  let tg = type_guard env in
  let ts = type_stm env in

  match s with
  | Ast.UAsn (loc, n, e) -> Ast.Asn (loc, n, te loc n e)
  | Ast.UAsrt (l, g) -> Ast.Asrt (l, tg g)
  | Ast.USeq (l, s1, s2) -> Ast.Seq (l, ts s1, ts s2) 
  | Ast.UIte (l, g, s1, s2) -> Ast.Ite (l, tg g, ts s1, ts s2) 
  | Ast.UWhile (l, g, s) -> Ast.While (l, tg g, ts s) 
