(** Congruence abstract domain. *)

type base_t = Bot | C of (int * int)

(* Remainder of the euclidien division: rem a b returns a positive value < b: *)
val rem: int -> int -> int

include NonRelational.Domain with type t = base_t
