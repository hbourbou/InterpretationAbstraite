module type Reduction = sig
  type t 
  val rho : t -> t
end
module  R = struct
  type t 
  let rho x  = match x with
	|Bot, _ -> (Bot,Bot)
	|_, Bot -> (Bot,Bot)
	|(Null, Top) -> (Null, Even)
	|(Null, Odd) -> (Bot,Bot)
end

module Make (D : NonRelational.Domain) (R : Reduction with type t = D.t) = struct
  type t = D.t
(* a printing function (useful for debuging), *)
let fprint ff x = (D.fprint ff x)

(* the order of the lattice. *)
let order x y = (D.order (R.rho x) (R.rho y))

(* and infimums of the lattice. *)
let top = D.top
let bottom = D.bottom

(* All the functions below are safe overapproximations.
 * You can keep them as this in a first implementation,
 * then refine them only when you need it to improve
 * the precision of your analyses. *)

let join x y = R.rho (D.join  x y)

let meet x y = R.rho (D.meet  x y)

let widening = join  (* Ok, maybe you'll need to implement this one if your
                      * lattice has infinite ascending chains and you want
                      * your analyses to terminate. *)

let sem_itv n1 n2 =  R.rho (D.sem_itv  n1 n2)

let sem_plus x y = R.rho (D.sem_plus  x y)
let sem_minus x y = R.rho (D.sem_minus  x y)
let sem_times x y = R.rho (D.sem_times  x y)
let sem_div x y = R.rho (D.sem_div  x y)

let sem_guard x = R.rho (D.sem_guard  x)

let backsem_plus x y r = x, y
let backsem_minus x y r = x, y
let backsem_times x y r = x, y
let backsem_div x y r = x, y
end
