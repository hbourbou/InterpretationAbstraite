(* Template to write your own non relational abstract domain. *)

(* To implement your own non relational abstract domain,
 * first give the type of its elements, *)
type t = Bot | Top | Pos | Neg |Null


(* a printing function (useful for debuging), *)
let fprint ff = function
  | Bot -> Format.fprintf ff "bot"
  | Top -> Format.fprintf ff "top"
  | Pos -> Format.fprintf ff ">=0" 
  | Neg -> Format.fprintf ff "<=0"
  | Null -> Format.fprintf ff "0"

(* the order of the lattice. *)
let order x y = 
  match x, y with
  | _ , Top -> true
  | Bot , _ -> true
  | Null, _ -> if y = Bot then false else true
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
  | Null, _ -> y
  | _ , Null -> x
  | _ -> if x=y then x else Top


let meet x y = match x, y with
  | _, Top -> x
  | Top, _ -> y
  | _, Bot -> Bot
  | Bot, _ -> Bot
  | Null, _ -> Null
  | _ , Null -> Null
  | _ -> if x=y then x else Null


let widening = join  (* Ok, maybe you'll need to implement this one if your
                      * lattice has infinite ascending chains and you want
                      * your analyses to terminate. *)

let sem_itv (n1:int) (n2:int) =
	if n2 < n1 then Bot 
    else
		if n1 = n2 then
					if n1=0 then Null 
					else 
						if n1 > 0 then Pos else  Neg
		else
			if n1 >= 0 then Pos
			else 
				if n2 <=0 then Neg
				else Top

    

let sem_plus x y = 
  match x,y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> Top
  | _, Top -> Top
  | Null, _ -> y
  | _ , Null -> x
  | _ -> if x= y then x else Top

let sem_minus x y = 
  match x,y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> Top
  | _, Top -> Top
  |Null , Null -> Null
  | Null, _ -> if y=Neg then Pos else Neg
  | _ , Null -> x
  | _ -> if x= y then Top else x
let sem_times x y = 
	match x,y with
	  | Bot, _  -> Bot
	  | _, Bot  -> Bot
	  |Null , _ -> Null
	  | _ , Null-> Null
	  | Top, _ -> Top
	  | _, Top -> Top
	  |  _ -> if x= y then Pos else Neg
let sem_div x y = 
	match x,y with
	  | Bot, _  -> Bot
	  | _, Bot  -> Bot
	  |Null , _ -> Null
	  | _ , Null-> Bot
	  | Top, _ -> Top
	  | _, Top -> Top
	  |  _ -> if x= y then Pos else Neg
let sem_guard x = if x = Null or x = Neg or x = Bot then Bot else Pos 

let backsem_plus x y r = x, y
let backsem_minus x y r = x, y
let backsem_times x y r = x, y
let backsem_div x y r = x, y
