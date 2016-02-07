
type intervalleWithError = Bot | Itv of (Q.t * Q.t)*(Q.t*Q.t)
include NonRelational.Domain with type t = intervalleWithError
