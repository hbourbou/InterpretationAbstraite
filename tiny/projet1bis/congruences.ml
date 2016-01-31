(*************************************************************************)
(*         Useful functions: use these functions for your work           *)
(*************************************************************************)

(* Compute the greatest common divisor *)
let gcd a b = match a = 0, b = 0 with
  | true, _ -> b
  | _, true -> a
  | _ -> Z.to_int (Z.gcd (Z.of_int a) (Z.of_int b))

(* Compute the least common multiple. *)
let lcm a b = match a=0, b=0 with
  | true, _ -> 0
  | _, true -> 0
  | _ -> Z.to_int (Z.lcm (Z.of_int a) (Z.of_int b))

(* Compute the euclidian remainder of a / b. Returns a value in [0, b-1] *)
let rem a b =  
  let r = a mod b in
  if r < 0 then r + b else r
    
(* check whether a and b have the same reminder modulo m *)
let eq_cong a b m = (m=0 && a=b) || (m<>0 && (rem a m = rem b m))




(* A congruence domain element is a pair (a,b) in Z x N.
   It denotes the set a Z + b *)
type base_t = Bot | C of (int * int)
type t = base_t
    
let norm t =
  match t with
  | Bot -> t 
  | C(a, b) -> if a = 0 then t else 
      C(abs a, rem b a)

  (* a printing function (useful for debuging), *)
  let fprint ff = function
    | Bot -> Format.fprintf ff "Bot" 
    | C (a,b) -> 
      if a = 0 then 
	Format.fprintf ff "%i" b 
      else if a=1 then
	Format.fprintf ff "Top"
      else
	Format.fprintf ff "%i[%i]" b a

  (* the order of the lattice. *)

    (* FONCTION A IMPLEMENTER *)
  let order x y =  assert false

  (* and supremums, infimums of the lattice. *)

  (* FONCTION A IMPLEMENTER *)
  let top = assert false

  (* FONCTION A IMPLEMENTER *)
  let bottom = assert false


  (* FONCTION A IMPLEMENTER *)    
  let join x y = assert false

  (* FONCTION A IMPLEMENTER *)
  let meet x y = assert false

  (* No widening available *)
  let widening = join 

  (* FONCTION A IMPLEMENTER *)
  let sem_itv n1 n2 = assert false

  (* FONCTION A IMPLEMENTER *)
  let sem_plus x y  = assert false

  (* FONCTION A IMPLEMENTER *)
  let sem_minus x y  = assert false


  (* FONCTION A IMPLEMENTER *)
  let sem_times x y  = assert false


  (* FONCTION A IMPLEMENTER *)
  let sem_div x y = assert false

  let sem_guard = function
    | t -> t

  let backsem_plus x y r = sem_minus r x, sem_minus r y
  let backsem_minus x y r = sem_plus r x, sem_plus r y
  let backsem_times x y r = x, y
  let backsem_div x y r = x, y

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
