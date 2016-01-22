(* Template to write your own non relational abstract domain. *)

(* To implement your own non relational abstract domain,
 * first give the type of its elements, *)
type t = Bot | Top | Odd | Even
      (* Cst of int *)

(* a printing function (useful for debuging), *)
let fprint ff = function
  | Bot -> Format.fprintf ff "bot"
  | Top -> Format.fprintf ff "top"
  | Odd -> Format.fprintf ff "odd"
  | Even -> Format.fprintf ff "even"
  (* | Cst v -> Format.fprintf ff "Cst %i" v *)

(* the order of the lattice. *)
let order x y = 
  match x, y with
  | _ , Top -> true
  | Bot , _ -> true
  | _ -> x = y 


(* and infimums of the lattice. *)
let top = Top
let bottom = Bot

(* All the functions below are safe overapproximations.
 * You can keep them as this in a first implementation,
 * then refine them only when you need it to improve
 * the precision of your analyses. *)

let join x y = match x, y with
  | _, Top -> Top
  | Top, _ -> Top
  | _, Bot -> x
  | Bot, _ -> y
  | _ -> if x=y then x else Top


let meet x y = match x, y with
  | _, Top -> x
  | Top, _ -> y
  | _, Bot -> Bot
  | Bot, _ -> Bot
  | _ -> if x=y then x else Bot


let widening = join  (* Ok, maybe you'll need to implement this one if your
                      * lattice has infinite ascending chains and you want
                      * your analyses to terminate. *)

let sem_itv (n1:int) (n2:int) =
  if n1 = n2 then
    (if n1 mod 2 = 0 then Even else Odd)
  else
    if n2 < n1 then Bot else Top

let sem_plus x y = 
  match x,y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> Top
  | _, Top -> Top
  | _ -> if x= y then Even else Odd

let sem_minus x y = sem_plus x y
let sem_times x y = 
	match x,y with
	  | Bot, _  -> Bot
	  | _, Bot  -> Bot
	  | Even, _ -> Even
	  | _, Even -> Even
	  | Odd, Odd -> Odd
	  | _ -> Top
let sem_div x y = match x,y with
	  | Bot, _  -> Bot
	  | _, Bot  -> Bot
	  | _ -> Top
let sem_guard x = x

let backsem_plus x y r = x, y
let backsem_minus x y r = x, y
let backsem_times x y r = x, y
let backsem_div x y r = x, y
