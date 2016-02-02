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
  | Top 			-> Format.fprintf ff "(-oo, +oo)"
  | Bounded (n1,n2) -> Format.fprintf ff "[%d, %d]" n1 n2 
  | NplusOO  n 		-> Format.fprintf ff "[%d, +oo)" n
  | NminusOO n	    -> Format.fprintf ff "(-oo, %d]" n 

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
  | _, Top 							-> Top
  | Top, _ 							-> Top
  
  | _, Bot 							-> x
  | Bot, _ 							-> y
  
  |Bounded (n1,n2), Bounded (m1,m2) -> Bounded (min n1 m1, max n2 m2)
  
  |Bounded (n1,n2), NplusOO m 		-> NplusOO (min n1 m)
  |NplusOO m, Bounded (n1,n2) 		-> NplusOO (min n1 m)
  
  |Bounded (n1,n2), NminusOO m 		-> NminusOO (max n2 m)
  |NminusOO m, Bounded (n1,n2) 		-> NminusOO (max n2 m)
  
  |NplusOO m, NplusOO n 			-> NplusOO (min n m)
  
  |NminusOO n, NminusOO m 			-> NminusOO (max n m)
  | _ -> Top


let meet x y = match x, y with
  | _, Top 							-> x
  | Top, _ 							-> y
  
  | _, Bot 							-> Bot
  | Bot, _ 							-> Bot
  
  |Bounded (n1,n2), Bounded (m1,m2) -> if (n2 < m1) || (n1 > m2) then Bot
										else Bounded (max n1 m1, min n2 m2)
  
  |Bounded (n1,n2), NplusOO m 		-> if (n2 < m) then Bot else Bounded (max n1 m,n2)
  |NplusOO m, Bounded (n1,n2) 		-> if (n2 < m) then Bot else Bounded (max n1 m,n2)
  
  |Bounded (n1,n2), NminusOO m 		-> if (n1 > m) then Bot else Bounded (n1, min m n2)
  |NminusOO m, Bounded (n1,n2) 		-> if (n1 > m) then Bot else Bounded (n1, min m n2)
  
  |NplusOO m, NplusOO n 			-> NplusOO (max n m)
  |NminusOO n, NminusOO m 			-> NminusOO (min n m)
  |NplusOO n, NminusOO m 			-> if n > m then Bot else Bounded (n,m)
  |NminusOO m, NplusOO n 			-> if n > m then Bot else Bounded (n,m)


let widening x y=  match x, y with
  | _, Top 							-> Top
  | Top, _ 							-> Top
  
  | _, Bot 							-> x
  | Bot, _ 							-> y
  
  |Bounded (a,b), Bounded (c,d) -> if (a <= c && d <= b) then x
										else if (a <= c && d > b) then NplusOO a
										else if (a > c && d <= b) then NminusOO b
										else Top

  
  |Bounded (n1,n2), NplusOO m 		-> if (n1 <= m) then NplusOO n1 else Top
  |NplusOO m, Bounded (n1,n2) 		-> if (n1 >= m) then NplusOO m  else Top
  
  |Bounded (n1,n2), NminusOO m 		-> if (n2 >= m) then NminusOO n2 else Top
  |NminusOO m, Bounded (n1,n2) 		-> if (n2 <= m) then NminusOO m  else Top
  
  |NplusOO m, NplusOO n 			-> if (m<=n) then NplusOO m else Top
  
  |NminusOO n, NminusOO m 			-> if (n >= m) then NminusOO n else Top
  | _ -> Top
let sem_itv (n1:int) (n2:int) = if n2 < n1 then Bot else Bounded (n1,n2)

let sem_plus x y = 
  match x,y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> Top
  | _, Top -> Top
  |Bounded (n1,n2), Bounded (m1,m2) ->  Bounded (n1 + m1, n2 + m2)
  
  |Bounded (n1,n2), NplusOO m 		-> NplusOO (n1 + m) 
  |NplusOO m, Bounded (n1,n2) 		-> NplusOO (n1 + m) 
  
  |Bounded (n1,n2), NminusOO m 		-> NminusOO (n2 + m )	
  |NminusOO m, Bounded (n1,n2) 		-> NminusOO (n2 + m )	
  
  |NplusOO m, NplusOO n 			-> NplusOO (n + m)
  
  |NminusOO n, NminusOO m 			-> NminusOO (n + m)
  | _ -> Top

let sem_minus x y =
  let minus z = 
		match z with
		  | Bot 			-> Bot
		  | Top 			-> Top
		  | Bounded (n1,n2) -> Bounded (-n2, -n1)
		  | NplusOO  n 		-> NminusOO (-n)
		  | NminusOO n	    -> NplusOO (-n)
  in
    sem_plus x (minus y)
  
let sem_times x y = 
  match x,y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> Top
  | _, Top -> Top
  |Bounded (n1,n2), Bounded (m1,m2) ->  let a = min (min (n1*m1) (n1*m2)) (min (n2*m1) (n2*m2)) in
										let b = max (max (n1*m1) (n1*m2)) (max (n2*m1) (n2*m2)) in
										Bounded (a, b)
  
  |NplusOO m, Bounded (n1,n2) 
  |Bounded (n1,n2), NplusOO m 		-> if n1 < 0 then 
												if n2 < 0 then 
															NminusOO (max (n1*m) (n2*m)) 
												else Top
									   else if n1 = 0 then
														if n2 = 0 then Bounded (0,0)
														else
														let a = min 0 (n2*m) in
														NplusOO a
											else 
												let a = min (n1*m) (n2*m) in
												NplusOO a

  
  |Bounded (n1,n2), NminusOO m 		
  |NminusOO m, Bounded (n1,n2) 		-> if n1 > 0 then NminusOO (max (n1*m) (n2*m)) 
									   else if n1 = 0 then
														if n2 = 0 then Bounded (0,0)
														else
														let b = max 0 (n2*m) in
														NminusOO b
											else 
												if n2 < 0 then 
														let a = min (n1*m) (n2*m) in
														NplusOO a	
												else Top
  
  |NplusOO m, NplusOO n 			-> if (n < 0) || (m < 0) then Top else NplusOO (n * m)
  
  |NminusOO n, NminusOO m 			-> if (n > 0) || (m > 0) then Top else NplusOO (n * m)
  | _ -> Top
	  
let rec sem_div x y =
  match x,y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> Top
  | _, Top -> Top
  |Bounded (n1,n2), Bounded (m1,m2) ->  if ((m1=0) && (m2=0)) then Bot
										else
											if ((m1 <= (-1)) && (m2 >= 1))  then (join (sem_div  x (Bounded(m1,-1))) (sem_div  x (Bounded(1,m2)))) 
											else
												let a = min (min (n1/m1) (n1/m2)) (min (n2/m1) (n2/m2)) in
												let b = max (max (n1/m1) (n1/m2)) (max (n2/m1) (n2/m2)) in
												Bounded (a, b)
  
  |NplusOO m, Bounded (n1,n2) 		-> if ((n1=0) && (n2=0)) then Bot
										else
											if ((n1 <= (-1)) && (n2 >= 1))  then (join (sem_div  x (Bounded(n1,-1))) (sem_div  x (Bounded(1,n2)))) 
										else
										if n1 > 0 then  let a = min (m/n1) (m/n2) in NplusOO a
									    else let b = max (m/n1) (m/n2) in
										NminusOO b	
  |Bounded (n1,n2), NplusOO m 		->  if (m=0) then Bot
										else
											if (m <= (-1))  then (join (sem_div  x (Bounded(m,-1))) (sem_div  x (NplusOO 1 ))) 
										else
										let a = min (min (n1/m) 0) (min (n2/m) 0) in
										let b = max (max (n1/m) 0) (max (n2/m) 0) in
										Bounded (a, b)
  
  |Bounded (n1,n2), NminusOO m 		-> if (m=0) then Bot
										else
											if (m >= 1)  then (join (sem_div  x (Bounded(1,m))) (sem_div  x (NminusOO (-1) ))) 
										else
										let a = min (min (n1/m) 0) (min (n2/m) 0) in
										let b = max (max (n1/m) 0) (max (n2/m) 0) in
										Bounded (a, b)
  |NminusOO m, Bounded (n1,n2) 		-> if ((n1=0) && (n2=0)) then Bot
										else
											if ((n1 <= (-1)) && (n2 >= 1))  then (join (sem_div  x (Bounded(n1,-1))) (sem_div  x (Bounded(1,n2))))  
										else
										if n1 > 0 then  let b = max (m/n1) (m/n2) in NminusOO b
									    else let a = min (m/n1) (m/n2) in
										NplusOO a
  
  |NplusOO n, NplusOO m 			-> if (m=0) then Bot
										else
											if (m <= (-1))  then (join (sem_div  x (Bounded(m,-1))) (sem_div  x (NplusOO 1 ))) 
										else
										let a = (min (n/m) 0) in NplusOO a
										
  
  |NminusOO n, NminusOO m 			-> if (m=0) then Bot
										else
											if (m >= 1)  then (join (sem_div  x (Bounded(1,m))) (sem_div  x (NminusOO (-1) ))) 
										else
										let a = (min (n/m) 0) in NplusOO a

  | NminusOO n, NplusOO m  			-> if (m=0) then Bot
										else
											if (m <= (-1))  then (join (sem_div  x (Bounded(m,-1))) (sem_div  x (NplusOO 1 ))) 
										else
										let b = max (n/m) 0 in NminusOO b
  |NplusOO n, NminusOO m 			-> if (m=0) then Bot
										else
											if (m >= 1)  then (join (sem_div  x (Bounded(1,m))) (sem_div  x (NminusOO (-1) ))) 
										else
										let b = max (n/m) 0 in NminusOO b
let sem_guard = meet (NplusOO 1)

let backsem_plus x y r = meet x (sem_minus r y), meet y (sem_minus r x)

let backsem_minus x y r = meet x (sem_plus y r), meet y (sem_minus x r)
let backsem_times x y r = x, y
let backsem_div x y r = x, y
