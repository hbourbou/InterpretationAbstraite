(* Template to write your own non relational abstract domain. *)

(* To implement your own non relational abstract domain,
 * first give the type of its elements, *)
type t = Bot | Top | Bounded of int*int | NplusOO of int | NminusOO of int
(* Bot = empty set
   Top = ]-oo,+oo[
   Bounded = [n1,n2]
   NplusOO = [n1,+oo[
   NminusOO = ]-OO,n]
*)

(* a printing function (useful for debuging), *)
let fprint ff = function
  | Bot 			-> Format.fprintf ff "bot"
  | Top 			-> Format.fprintf ff "top"
  | Bounded (n1,n2) -> Format.fprintf ff "[%i,%i]" n1 n2 
  | NplusOO  n 		-> Format.fprintf ff "[%i,+oo[" n
  | NminusOO n	    -> Format.fprintf ff "]-oo,%i]" n 

(* the order of the lattice. *)
let order x y = 
  match x, y with
  | _ , Top -> true
  | Bot , _ -> true
  |Bounded (n1,n2), Bounded (m1,m2) -> (n1 >= m1) && (n2 <= m2)
  |Bounded (n1,n2), NplusOO m 		-> n1 >= m
  |Bounded (n1,n2), NminusOO m 		-> n2 <= m
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
    Cst n1
  else
    if n2 < n1 then Bot else Top

let sem_plus x y = 
  match x,y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> Top
  | _, Top -> Top
  | Cst n1, Cst n2 -> Cst (n1+n2)

let sem_minus x y =
  match x,y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> Top
  | _, Top -> Top
  | Cst n1, Cst n2 -> Cst (n1-n2)
  
let sem_times x y = 
  match x,y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> Top
  | _, Top -> Top
  | Cst n1, Cst n2 -> Cst (n1*n2)
	  
let sem_div x y =
 match x,y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> Top
  | _, Top -> Top
  | Cst n1, Cst n2 -> if n2 = 0 then Bot else Cst (n1/n2)
	  
let sem_guard = 
	function
  | Bot -> Bot
  | Top -> Top
  | Cst v -> if v > 0 then Cst v else Bot

let backsem_plus x y r = x, y
let backsem_minus x y r = x, y
let backsem_times x y r = x, y
let backsem_div x y r = x, y
