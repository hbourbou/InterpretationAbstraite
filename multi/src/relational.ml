(*
 * TINY (Tiny Is Not Yasa (Yet Another Static Analyzer)):
 * a simple abstract interpreter for teaching purpose.
 * Copyright (C) 2012, 2014  P. Roux
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

(** A module type for relational domains. *)

(** Module type for relational domains. *)
module type Domain = sig
  (** Name of the domain. Used to select it via command line
      arguments. Should be of the form \[a-z\]\[a-zA-Z0-9_\]*. *)
  val name : string

  (** [parse_param s] parses parameters in [s]. If [s] is of the form
      "dom:s'" where dom doesn't contain ':' and is not [name], the
      parameter should be ignored. If dom is [name], only s' should be
      considered ([Utils.select_param name] can be used for that). A
      warning should be issued for unrecognised parameters (use
      [Utils.warn_unknown_param]). *)
  val parse_param : string -> unit

  (** Outputs some help about the options recognized by
      [parse_param]. *)
  val fprint_help : Format.formatter -> unit

  (** Type of abstract values. *)
  type t

  (** Prints an abstract value. *)
  val fprint : Format.formatter -> t -> unit

  (** {2 Lattice Structure} *)

  (** Order on type [t]. [t] with this order must be a lattice. *)
  val order : t -> t -> bool

  val top : Name.Set.t -> t
  val bottom : Name.Set.t -> t
  val is_bottom : t -> bool

  val get_vars: t -> Name.Set.t
  (** Infimums of the lattice (when the relational domain focuses on given set
      of variables). *)

  val join : t -> t -> t
  val meet : t -> t -> t
  (** Least upper bound and greatest lower bound of the lattice. *)

  (** Widening to ensure termination of the analyses. *)
  val widening : t -> t -> t

  (** {2 Abstract Operators} *)

  (** Abstract semantics of assignments and guards. *)

  (** [assignment n e t] returns a [t'] such that:
      {[[|n = e;|](\gamma(t)) \subseteq \gamma(t').]} *)
  val assignment : Name.t -> Ast.expr -> t -> t

  (** [guard e t] returns a [t'] such that:
      {[[|e >= 0|](\gamma(t)) \subseteq \gamma(t').]}*)
  val guard : Ast.expr -> t -> t
end

(** Product functor for relational domains. *)
module Prod (D1: Domain) (D2: Domain) : Domain =
struct

  let name = "_"
  let parse_param _ = ()
  let fprint_help fmt = ()
  type t = D1.t * D2.t
  let fprint fmt (x,y) = Format.fprintf fmt "%a, %a" D1.fprint x D2.fprint y
  let order (x1, y1) (x2, y2) = D1.order x1 x2 && D2.order y1 y2
  let top s = D1.top s, D2.top s
  let bottom s = D1.bottom s, D2.bottom s
  let get_vars (x,y) = Name.Set.union (D1.get_vars x ) (D2.get_vars y)
  let is_bottom (x,y) = D1.is_bottom x || D2.is_bottom y
  let join (x1, y1) (x2, y2) = D1.join x1 x2, D2.join y1 y2
  let meet ((x1, y1) as e1) ((x2, y2) as e2) = 
    if is_bottom e1 || is_bottom e2 then 
      bottom  (Name.Set.union (get_vars e1) (get_vars e2))
    else 
      D1.meet x1 x2, D2.meet y1 y2
  let widening (x1, y1) (x2, y2) = D1.widening x1 x2, D2.widening y1 y2
  let assignment v e (x,y) = D1.assignment v e x, D2.assignment v e y
  let guard e (x,y) = D1.guard e x, D2.guard e y
end
