(*
 * TINY (Tiny Is Not Yasa (Yet Another Static Analyzer)):
 * a simple abstract interpreter for teaching purpose.
 * Copyright (C) 2012  P. Roux
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

module Domain : NonRelational.Domain = struct
  type t = Bot | Cst of int | Top

  let fprint ff = function
    | Bot -> Format.fprintf ff "_|_"
    | Cst n -> Format.fprintf ff "%d" n
    | Top -> Format.fprintf ff "T"

  let order x y = match x, y with
    | Bot, _ -> true
    | _, Top -> true
    | Cst n1, Cst n2 -> n1 = n2
    | _ -> false

  let top = Top
  let bottom = Bot
  let join x y = match x, y with
    | Top, _ | _, Top -> Top
    | Bot, _ -> y
    | _, Bot -> x
    | Cst n1, Cst n2 -> if n1 = n2 then x else Top
  let meet x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Top, _ -> y
    | _, Top -> x
    | Cst n1, Cst n2 -> if n1 = n2 then x else Bot
  let widening = join

  let sem_itv n1 n2 = if n1 > n2 then Bot else if n1 = n2 then Cst n1 else Top
  (* TODO: check for overflows *)
  let sem_guard = function
    | Bot -> Bot
    | (Cst n) as c -> if n > 0 then c else Bot
    | Top -> Top
  let sem_op op x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Top, _ | _, Top -> Top
    | Cst n1, Cst n2 -> Cst (op n1 n2)
  let sem_plus = sem_op ( + )
  let sem_minus = sem_op ( + )
  let sem_times = sem_op ( + )
  let sem_div x y = try sem_op ( / ) x y with Division_by_zero -> Bot
  (* We could have more precise backward abstract semantics,
   * but that would be completely useless. *)
  let backsem_plus x y r = x, y
  let backsem_minus x y r = x, y
  let backsem_times x y r = x, y
  let backsem_div x y r = x, y
end
