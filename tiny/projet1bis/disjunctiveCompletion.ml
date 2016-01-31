(* This file contains two functors to build disjunctive complete domains 

   MakeKildall build a domain with the disjunctive completion of Kildall, up to
   a fixed size n, and rely on the abstract domain to manipulate more abstract values.

   Make is more general and manipulates only set of abstract values.

*)

(* Dans la suite, on pourra utiliser les fonctions suivantes du module Set d'Ocaml
   empty
   fold
   add
   singleton
   cardinal
   subset
   union
   inter
   filter
   is_empty
   exists
   for_all
   of_list
*)


module MakeKildall (D: NonRelational.Domain) (Size: sig val n: int end)=
struct

  (* On construit le Set des ensembles d'entiers *) 
  module Set = Set.Make (
    struct 
      type t = int 
      let compare = compare
    end)

  type t = FiniteSet of Set.t | Bigger of D.t

  let fprint fmt x =
    match x with
      FiniteSet s -> Format.fprintf fmt "{%a }" 
	(Utils.fprintf_list ~sep:", " Format.pp_print_int) 
	(Set.elements s)
    | Bigger b -> D.fprint fmt b

  (*********** Utils ***************)

  (* Convertit un ensemble d'entier dans le plus petit élément de D qui les
     contient *)
  let set_to_d s = assert false (* A FAIRE *)

  (* Fonction récursive qui construit la liste des entiers entre a et b *)
  let rec list_itv a b =
    if a <= b then
      a :: (list_itv (a+1) b)
    else []

  (* Normalise un élément abstrait si l'ensemble contient trop d'éléments (trop
     d'entiers) *)
  let normalize s = assert false (* A FAIRE *)

	
  (*********** Lattice ***************)
  let order x y = assert false (* A FAIRE *)

  let top = assert false (* A FAIRE *)

  let bottom = assert false (* A FAIRE *)

  let join x y = assert false (* A FAIRE *)

  let meet x y = assert false (* A FAIRE *)

  let widening x y = assert false (* A FAIRE *)


  (*********** Abs Sem ***************)      
  let sem_itv a b = assert false (* A FAIRE *)

  (* La fonction arith (f_conc, f_abs) x y répresente l'application d'un
     opérateur binaire f aux arguments x y.  f_conc est l'opérateur concret (sur
     des entiers) et f_abs sa version abstraite dans D.  Les types attendus sont
     inscrits dans la signature.  *)
  let arith ((conc_fun: int -> int -> int), (abs_fun: D.t -> D.t -> D.t)) 
            (x:t) 
	    (y:t) : t = 
    assert false (* A FAIRE *)

      
  (* Si on utilise la fonction arith ci-dessus, la fonction (+) représente la
     fonction d'addition des entiers. Idem pour (-), ( * ) et (/) *)

  let sem_plus x y =  assert false (* A FAIRE *)
  let sem_minus x y = assert false (* A FAIRE *)
  let sem_times x y = assert false (* A FAIRE *)
  let sem_div x y = assert false (* A FAIRE *)

  let sem_guard x = assert false (* A FAIRE *)

  let backmeet x y (x', y') = assert false (* A FAIRE *)
  let backsem_plus x y r = assert false (* A FAIRE *)
  let backsem_minus x y r = assert false (* A FAIRE *)
  let backsem_times x y r = assert false (* A FAIRE *)
  let backsem_div x y r = assert false (* A FAIRE *)
end


(* More general setting manipulating only sets of abstract values *)
module Make (D: sig include NonRelational.Domain val is_join_exact: t -> t -> bool end) 
            (Size: sig val n: int end) =
struct

  (* Set denote ici un ensemble de valeurs de D *)
  module Set = 
  struct
    include Set.Make (
      struct 
	type t = D.t 
	let compare = compare
      end)
end

  type t = Set.t

  let fprint fmt s =
    if Set.is_empty s then
      Format.fprintf fmt "Bot" 
    else
    Format.fprintf fmt "{%a }" 
      (Utils.fprintf_list ~sep:", " D.fprint)
      (Set.elements s)

  (*********** Utils ***************)

  let is_equal e1 e2 = 
    D.order e1 e2 && D.order e2 e1

  let rec set_itv a b =
    if a = b then
      Set.singleton (D.sem_itv a a)
    else if a < b then
      Set.add (D.sem_itv a a) (set_itv (a+1) b)
    else
      Set.empty (* should only happen for incorrect itv *)

  (* Meme chose qu'au dessus. On prendra soin de maintenir une forme normale sur
     les élements de l'ensemble. Par exemple, sur des intervalles où l'union
     peut être exacte, l'ensemble { [-1,1] [2, 3] pourra être normalisé sans
     perte d'informations par [-1, 3]. Ou plus générale, y compris sans join
     exact: { [-1,0], [0,0] } deviendra { [-1, 0]} *)

	
  let normalize s =  assert false (* A FAIRE *)
    

  (*********** Lattice ***************)
  let order x y = assert false (* A FAIRE *)

  let top = assert false (* A FAIRE *)

  let bottom = assert false (* A FAIRE *)

  let join x y = assert false (* A FAIRE *)

  let meet x y = assert false (* A FAIRE *)

  let widening x y = assert false (* A FAIRE *)


  (*********** Abs Sem ***************)      
  let sem_itv a b = assert false (* A FAIRE *)

  let arith (abs_fun: D.t -> D.t -> D.t)
            (x:t) 
	    (y:t) : t = 
    assert false (* A FAIRE *)

      
  (* Si on utilise la fonction arith ci-dessus, la fonction (+) représente la
     fonction d'addition des entiers. Idem pour (-), ( * ) et (/) *)

  let sem_plus x y =  assert false (* A FAIRE *)
  let sem_minus x y = assert false (* A FAIRE *)
  let sem_times x y = assert false (* A FAIRE *)
  let sem_div x y = assert false (* A FAIRE *)

  let sem_guard x = assert false (* A FAIRE *)

  let backmeet x y (x', y') = assert false (* A FAIRE *)
  let backsem_plus x y r = assert false (* A FAIRE *)
  let backsem_minus x y r = assert false (* A FAIRE *)
  let backsem_times x y r = assert false (* A FAIRE *)
  let backsem_div x y r = assert false (* A FAIRE *)

end

module FiniteSet (Size: sig val n: int end) = MakeKildall (BotTop) (Size)

module Itv (Size: sig val n: int end) = 
  Make 
    (struct
      include Intervals

      (* The join is exact iff the two intervals can be concatenated *)
      let is_join_exact x y = match x, y with
	  Intervals.Itv (a,b), Intervals.Itv (c,d) -> 
	    let (<=) = InfInt.order in
	    let a_minus_one = InfInt.sub_lb a InfInt.one in
	    let b_plus_one = InfInt.add_ub b InfInt.one in
	    (* Case 1 *)
	    (a_minus_one <= c && c <= b_plus_one)
	    || 
	      (* Case 2 *)
	      (a_minus_one <= d && d <= b_plus_one)
	| _ -> false
	    
     end) 
    (Size)
