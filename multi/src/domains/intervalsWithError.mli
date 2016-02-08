
type intervalleWithError = Bot | Itv of (float * float)*(float*float)
include NonRelational.Domain with type t = intervalleWithError
