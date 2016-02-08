let name = "intervalWithError"

let base_type = Ast.RealT

(* no option *)
let parse_param _ = ()

let fprint_help fmt = Format.fprintf fmt "Interval abstraction"

type intervalleWithError = Bot | Itv of (float * float)*(float*float)
(* Itv ((x,ex);(y,ey))*)
type t = intervalleWithError


let fprint ff = function
  | Bot -> Format.fprintf ff "⊥"
  | Itv ((a, ea),(b,eb)) -> Format.fprintf ff "%s%s%s, %s%s; %s%s, %s%s%s"
    (if (classify_float a) = FP_infinite  then "(" else "[")
   ("(")(string_of_float a) (string_of_float ea)(")")
   ("(")(string_of_float b) (string_of_float eb) (")")
   (if (classify_float b) = FP_infinite then ")" else "]")
let order x y = match x, y with
  | Bot, _ -> true
  | _, Bot -> false
  | Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed)) ->  c <= a &&  b <= d

let top = Itv ((neg_infinity,  0.0),(infinity,0.0))
let bottom = Bot
let is_bottom x = x = Bot

let join x y = match x, y with
  | Bot, _ -> y
  | _, Bot -> x
  | Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed))-> 
			let e1 = if ( a <= c) then ea else ec
			in 
			let e2 = if ( b >= d) then eb else ed
			in Itv ((min a c,e1),( max b d,e2))



let meet x y = match x, y with
  | Bot, _
  | _, Bot -> Bot
  | Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed)) -> 
			let e1 = if ( a >= c) then ea else ec
			in 
			let e2 = if (b <= d) then eb else ed
			in Itv ((max a c,e1),( min b d,e2))
let widening x y = match x, y with
  | Bot, _ -> y
  | _, Bot -> x
  | Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed)) ->
    let e = if  a <= c then (a,ea) else (neg_infinity, 0.0) in
    let f = if d  <= b then (b,eb) else (infinity, 0.0) in
    Itv (e, f)
(* [mk_itv i1 i2] returns the interval (Itv ((i1,ei1),(i2,ei2))) when i1 <= i2,
   Bot otherwise. *)
(*rounOffError n'est pas correcte mais elle crée un erreur*)
let roundOffError x = abs_float(x)*.(2.**(-53.)) 

let sem_itv (q1,s1) (q2,s2) = if Q.leq q1  q2 then 
			let e1 = roundOffError (float_of_string s1) 
			in 
			let e2 = roundOffError (float_of_string s2) 
			in
			Itv ((float_of_string s1, e1),(float_of_string s2,e2)) 
		    else Bot

let sem_plus x y = match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  |  Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed))  -> 
		let e = (a +. c,  ( ea +. ec) +. (roundOffError (a +. c)))
		in
		let f = (b +. d, (eb +. ed) +. (roundOffError (b +. d)))
		in
		Itv (e, f)

let sem_minus x y = match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  |  Itv ((a, ea),(b,eb)), Itv ((c, ec),(d,ed))  -> 
		let e = (a -. d, (ea -. ed) +. (roundOffError (a -. d)))
		in
		let f = ( b -. c,  (eb -. ec) +. (roundOffError ( b -. c)))
		in
		Itv (e, f)
let ourMul (x,ex) (y,ey) = 
	(x *. y,  
		(( ex *. y) +. (ey *. x)) +.
		((ex *. ey) +. (roundOffError (y *. x)))
	)
let ourMin (x,ex) (y,ey) = if x <= y then (x,ex) else (y,ey)
let ourMax (x,ex) (y,ey) = if  x >= y then (x,ex) else (y,ey)
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
( ( 1.) /. y,roundOffError ( ( 1.) /. y))

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


let sem_geq0 = meet (Itv (( 0.,0.), (infinity, 0.)))

let backsem_plus x y r = meet x (sem_minus r y), meet y (sem_minus r x)

let backsem_minus x y r = meet x (sem_plus y r), meet y (sem_minus x r)

let backsem_times x y r =
  let backsem_times_left x y r =
    (* [contains_0 x] returns true iff the interval x contains 0 *)
    let contains_0 x = meet x (Itv (( 0.,0.),( 0.,0.))) <> Bot in
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


