module type Reduction = sig
  type t
  val rho : t -> t
end

module Make (D : NonRelational.Domain) (R : Reduction with type t = D.t) = struct
  type t = D.t

TODO
end
