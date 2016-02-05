(* Version correcte (sans débordement arithmétiques) de Intervals3. *)

module Make (I: sig val name_suffix: string val base_type : Ast.base_type end) =
struct
let name = "interval" ^ I.name_suffix

let base_type = I.base_type

(* no option *)
let parse_param _ = ()

let fprint_help fmt = Format.fprintf fmt "Interval abstraction"


type t = Bot | Itv of Q.t * Q.t
(* The module Infint extends 64 bits integers with -oo and +oo
 * with some arithmetic operations. *)

let fprint ff = function
  | Bot -> Format.fprintf ff "⊥"
  | Itv (a, b) -> Format.fprintf ff "%s%s, %s%s"
    (if Q.equal a Q.minus_inf then "(" else "[")
    (Q.to_string a) (Q.to_string b)
    (if Q.equal b Q.inf then ")" else "]")

let order x y = match x, y with
  | Bot, _ -> true
  | _, Bot -> false
  | Itv (a, b), Itv (c, d) -> Q.leq c a && Q.leq b d

let top = Itv (Q.minus_inf, Q.inf)
let bottom = Bot
let is_bottom x = x = Bot

let join x y = match x, y with
  | Bot, _ -> y
  | _, Bot -> x
  | Itv (a, b), Itv (c, d) -> Itv (Q.min a c, Q.max b d)

(* [mk_itv i1 i2] returns the interval (Itv (i1, i2)) when i1 <= i2,
   Bot otherwise. *)
let mk_itv i1 i2 = if Q.leq i1 i2 then Itv (i1, i2) else Bot

let meet x y = match x, y with
  | Bot, _
  | _, Bot -> Bot
  | Itv (a, b), Itv (c, d) -> mk_itv (Q.max a c) (Q.min b d)

let widening x y = match x, y with
  | Bot, _ -> y
  | _, Bot -> x
  | Itv (a, b), Itv (c, d) ->
    let e = if Q.leq a c then a else Q.minus_inf in
    let f = if Q.leq d b then b else Q.inf in
    Itv (e, f)

let sem_itv n1 n2 = mk_itv n1 n2

let sem_plus x y = match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Itv (a, b), Itv (c, d) -> Itv (Q.add a c, Q.add b d)

let sem_minus x y = match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Itv (a, b), Itv (c, d) -> Itv (Q.sub a d, Q.sub b c)

let sem_times x y = match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Itv (a, b), Itv (c, d) ->
    let e = Q.min
      (Q.min (Q.mul a c) (Q.mul b d))
      (Q.min (Q.mul b c) (Q.mul a d)) in
    let f = Q.max
      (Q.max (Q.mul a c) (Q.mul b d))
      (Q.max (Q.mul b c) (Q.mul a d)) in
    Itv (e, f)

let sem_div_real x y =
  match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Itv (a, b), Itv (c, d) ->
    let e = Q.min
      (Q.min (Q.div a c) (Q.div b d))
      (Q.min (Q.div b c) (Q.div a d)) in
    let f = Q.max
      (Q.max (Q.div a c) (Q.div b d))
      (Q.max (Q.div b c) (Q.div a d)) in
    Itv (e, f)

let sem_div_int x y =
  (* precondition: meet y [0, 0] = ⊥ *)
  let sem_div_without_0 x y = match x, y with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) ->
      let e = Q.min
        (Q.min (Q.div a c) (Q.div b d))
        (Q.min (Q.div b c) (Q.div a d)) in
      let f = Q.max
        (Q.max (Q.div a c) (Q.div b d))
        (Q.max (Q.div b c) (Q.div a d)) in
      Itv (e, f) in
  let yneg = meet y (Itv (Q.minus_inf, Q.of_int (-1))) in
  let ypos = meet y (Itv (Q.of_int 1, Q.inf)) in
  join (sem_div_without_0 x yneg) (sem_div_without_0 x ypos)

let sem_div x y =
  match base_type with
    Ast.IntT -> sem_div_int x y
  | Ast.RealT -> sem_div_real x y
  | _ -> assert false

let sem_geq0 = meet (Itv (Q.of_int 0, Q.inf))

let backsem_plus x y r = meet x (sem_minus r y), meet y (sem_minus r x)

let backsem_minus x y r = meet x (sem_plus y r), meet y (sem_minus x r)

let backsem_times x y r =
  let backsem_times_left x y r =
    (* [contains_0 x] returns true iff the interval x contains 0 *)
    let contains_0 x = meet x (Itv (Q.of_int 0, Q.of_int 0)) <> Bot in
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
end

module Int = Make (struct let name_suffix = "_int" let base_type = Ast.IntT end)
module Real = Make (struct let name_suffix = "_real" let base_type = Ast.RealT end)
