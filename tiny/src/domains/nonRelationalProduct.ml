
module Make (D1 : NonRelational.Domain) (D2 : NonRelational.Domain) = struct
  type t = D1.t * D2.t
(* a printing function (useful for debuging), *)
let fprint ff (x,y)= Format.fprintf ff "(%a, %a)" D1.fprint x D2.fprint y

(* the order of the lattice. *)
let order (x1,y1) (x2,y2) = (D1.order x1 x2) &&  (D2.order y1 y2)

(* and infimums of the lattice. *)
let top = (D1.top, D2.top)
let bottom = (D1.bottom, D2.bottom)

(* All the functions below are safe overapproximations.
 * You can keep them as this in a first implementation,
 * then refine them only when you need it to improve
 * the precision of your analyses. *)

let join (x1,y1) (x2,y2)  = ((D1.join x1 x2) ,  (D2.join y1 y2))

let meet (x1,y1) (x2,y2)  = ((D1.meet x1 x2) ,  (D2.meet y1 y2))

let widening = join  (* Ok, maybe you'll need to implement this one if your
                      * lattice has infinite ascending chains and you want
                      * your analyses to terminate. *)

let sem_itv n1 n2 = ((D1.sem_itv n1 n2) ,  (D2.sem_itv n1 n2))

let sem_plus (x1,y1) (x2,y2)  = ((D1.sem_plus x1 x2) ,  (D2.sem_plus y1 y2))
let sem_minus (x1,y1) (x2,y2)  = ((D1.sem_minus x1 x2) ,  (D2.sem_minus y1 y2))
let sem_times (x1,y1) (x2,y2) = ((D1.sem_times x1 x2) ,  (D2.sem_times y1 y2))
let sem_div (x1,y1) (x2,y2)  = ((D1.sem_div x1 x2) ,  (D2.sem_div y1 y2))

let sem_guard (x1,y1)= ((D1.sem_guard x1) ,  (D2.sem_guard y1))

let backsem_plus x y r = x, y
let backsem_minus x y r = x, y
let backsem_times x y r = x, y
let backsem_div x y r = x, y
end
