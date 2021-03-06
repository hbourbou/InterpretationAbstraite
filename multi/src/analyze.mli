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

(** Analyze TINY programs. *)

(** [analyze domain descending input output] analyzes RINY program in
    file [input] with abstract domain [domain] and outputs result as a
    commented program (or only invariants if [Report.verbosity] <= 1)
    in file [output] or standard output if no output is provided.

    After fixpoint of loops are reached, performs [descending]
    descending iterations. *)
val analyze :
  (module Relational.Domain) -> int -> string -> string option -> unit
