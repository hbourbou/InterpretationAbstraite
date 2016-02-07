let name = "intervalWithError"

let base_type = Ast.RealT

(* no option *)
let parse_param _ = ()

let fprint_help fmt = Format.fprintf fmt "Interval abstraction"

type intervalleWithError = Bot | Itv of (Q.t * Q.t)*(Q.t*Q.t)
(* Itv ((x,ex);(y,ey))*)
type t = intervalleWithError


let fprint ff = function
  | Bot -> Format.fprintf ff "⊥"
  | Itv ((a, ea),(b,eb)) -> Format.fprintf ff "%s%s%s, %s%s; %s%s, %s%s%s"
    (if Q.equal a Q.minus_inf then "(" else "[")
   ("(")(Q.to_string a) (Q.to_string ea)(")")
   ("(")(Q.to_string b) (Q.to_string eb) (")")
   (if Q.equal b Q.inf then ")" else "]")
let order x y = match x, y with
  | Bot, _ -> true
  | _, Bot -> false
  | Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed)) -> Q.leq c a && Q.leq b d

let top = Itv ((Q.minus_inf, Q.of_int 0),(Q.inf,Q.of_int 0))
let bottom = Bot
let is_bottom x = x = Bot

let join x y = match x, y with
  | Bot, _ -> y
  | _, Bot -> x
  | Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed))-> 
			let e1 = if (Q.leq a c) then ea else ec
			in 
			let e2 = if (Q.geq b d) then eb else ed
			in Itv ((Q.min a c,e1),( Q.max b d,e2))



let meet x y = match x, y with
  | Bot, _
  | _, Bot -> Bot
  | Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed)) -> 
			let e1 = if (Q.geq a c) then ea else ec
			in 
			let e2 = if (Q.leq b d) then eb else ed
			in Itv ((Q.max a c,e1),( Q.min b d,e2))
let widening x y = match x, y with
  | Bot, _ -> y
  | _, Bot -> x
  | Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed)) ->
    let e = if Q.leq a c then (a,ea) else (Q.minus_inf,Q.of_int 0) in
    let f = if Q.leq d b then (b,eb) else (Q.inf,Q.of_int 0) in
    Itv (e, f)
(* [mk_itv i1 i2] returns the interval (Itv ((i1,ei1),(i2,ei2))) when i1 <= i2,
   Bot otherwise. *)
(*rounOffError n'est pas correcte mais elle crée un erreur*)
let roundOffError x = Q.sub x  (Q.of_float (float (Q.to_int x)))
let mk_itv i1 i2 = if Q.leq i1  i2 then 
			let e1 = roundOffError i1 
			in 
			let e2 = roundOffError i2
			in
			Itv ((i1, e1),(i2,e2)) 
		    else Bot
let sem_itv n1 n2 = mk_itv n1 n2

let sem_plus x y = match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  |  Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed))  -> 
		let e = (Q.add a c, Q.add (Q.add ea ec) (roundOffError (Q.add a c)))
		in
		let f = (Q.add b d, Q.add (Q.add eb ed) (roundOffError (Q.add b d)))
		in
		Itv (e, f)

let sem_minus x y = match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  |  Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed))  -> 
		let e = (Q.sub a d, Q.add (Q.sub ea ed) (roundOffError (Q.sub a d)))
		in
		let f = ( Q.sub b c, Q.add (Q.sub eb ec) (roundOffError (Q.sub b c)))
		in
		Itv (e, f)
let ourMul (x,ex) (y,ey) = 
	(Q.mul x y, Q.add 
		(Q.add (Q.mul ex y) (Q.mul ey x))
		(Q.add (Q.mul ex ey) (roundOffError (Q.mul y x)))
	)
let ourMin (x,ex) (y,ey) = if Q.leq x y then (x,ex) else (y,ey)
let ourMax (x,ex) (y,ey) = if Q.geq x y then (x,ex) else (y,ey)
let sem_times x y = match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Itv (a, b), Itv (c, d) ->
    let e = ourMin
      (ourMin (ourMul a c) (ourMul b d))
      (ourMin (ourMul b c) (ourMul a d)) in
    let f = ourMax
      (ourMax (ourMul a c) (ourMul b d))
      (ourMax (ourMul b c) (ourMul a d)) in
    Itv (e, f)

let ourDiv (x,ex) (y,ey) = ourMul (x,ex) 
(Q.div (Q.of_float 1.) y,roundOffError (Q.div (Q.of_float 1.) y))

let sem_div_real x y =
  match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Itv (a, b), Itv (c, d) ->
    let e = ourMin
      (ourMin (ourDiv a c) (ourDiv b d))
      (ourMin (ourDiv b c) (ourDiv a d)) in
    let f = ourMax
      (ourMax (ourDiv a c) (ourDiv b d))
      (ourMax (ourDiv b c) (ourDiv a d)) in
    Itv (e, f)


let sem_div x y = sem_div_real x y


let sem_geq0 = meet (Itv ((Q.of_int 0,Q.of_int 0), (Q.inf,Q.of_int 0)))

let backsem_plus x y r = meet x (sem_minus r y), meet y (sem_minus r x)

let backsem_minus x y r = meet x (sem_plus y r), meet y (sem_minus x r)

let backsem_times x y r =
  let backsem_times_left x y r =
    (* [contains_0 x] returns true iff the interval x contains 0 *)
    let contains_0 x = meet x (Itv ((Q.of_int 0, Q.of_int 0),(Q.of_int 0,Q.of_int 0))) <> Bot in
    if contains_0 y && contains_0 r then
      x  (* When both y and r can be 0,
            x * y = r doesn't teach us anything about x. *)
    else
      meet x (sem_div r y) in
  backsem_times_left x y r, backsem_times_left y x r


let backsem_div x y r =
(* TODO *)
x, y
(*
  match base_type with
    Ast.IntT -> 
      (* The division being an euclidean division x / y = z doesn't implis x = z *
	 y, rather x = z * y + r with r \in [-|y|+1, |y|-1]. *)
      let remaining y = match y with
	| Itv (a, b) -> begin match Q.to_int a, Q.to_int b with
          | Some a, Some b ->
            let c = Q.of_int (max (abs a) (abs b)) in
            mk_itv (Q.sub Q.one c) (Q.sub c Q.one)
          | _ -> top end
	| _ -> top in
      let backsem_div_left x y r =
	meet x (sem_plus (sem_times r y) (remaining y)) in
      let backsem_div_right x y r =
	let x' = sem_plus x (remaining y) in
	meet y (fst (backsem_times y r x')) in
      backsem_div_left x y r, backsem_div_right x y r
  | Ast.RealT -> (* For real/rat, x / y = r. Then x = r * y and y = x / r *)
    meet x (sem_times r y), meet y (sem_div x r)
*)


