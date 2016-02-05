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

module Make (Dom : Relational.Domain) = struct
  let print m t output_filename =
    let top = Dom.top (Ast.vars_of_stm t) in
    let rec fprint_stm ff s =
      let fprint_annot f ff l =
        try
          let a = Location.Map.find l m in
          if Report.silent (fun () -> Dom.order top a) then ()
          else Format.fprintf ff f Dom.fprint a
        with Not_found -> () in
      let fprint_invariant = fprint_annot "/* loop invariant: %a */@ " in
      let fprint_annot = fprint_annot "@ /* %a */" in
      match s with
      | Ast.Asn (l, n, e) -> Format.fprintf ff "%s = @[%a@];%a"
        n Ast.fprint_expr e fprint_annot (Location.end_p l)
      | Ast.Asrt (l, g) -> Format.fprintf ff "assert(%a);%a"
        Ast.fprint_guard g fprint_annot (Location.end_p l)
      | Ast.Seq (_, s1, s2) ->
        Format.fprintf ff "@[<v>%a@ %a@]" fprint_stm s1 fprint_stm s2
      | Ast.Ite (l, g, s1, s2) ->
        Format.fprintf ff "@[<v>@[<v 2>if (%a) {%a@ %a@]@ @[<v 2>} else {%a@ %a@]@ }%a@]"
          Ast.fprint_guard g
          fprint_annot (Location.beg_p (Ast.loc_of_stm s1)) fprint_stm s1
          fprint_annot (Location.beg_p (Ast.loc_of_stm s2)) fprint_stm s2
          fprint_annot (Location.end_p l)
      | Ast.While (l, g, s) ->
        Format.fprintf ff "@[<v>%a@[<v 2>while (%a) {%a@ %a@]@ }%a@]"
          fprint_invariant (Location.beg_p (Ast.loc_of_guard g))
          Ast.fprint_guard g
          fprint_annot (Location.beg_p (Ast.loc_of_stm s)) fprint_stm s
          fprint_annot (Location.end_p l) in
    Utils.with_out_ch output_filename (fun out_ch ->
      let ff = Format.formatter_of_out_channel out_ch in
      fprint_stm ff t;
      Format.fprintf ff "\n%!");
    Report.nlogf 1 "Results written to %s."
      (Utils.output_filename_string output_filename)

  let print_invariants m t output_filename =
    let top = Dom.top (Ast.vars_of_stm t) in
    let rec fprint_stm ff s =
      let fprint_annot ff l =
        try
          let a = Location.Map.find l m in
          if Report.silent (fun () -> Dom.order top a) then raise Not_found
          else Format.fprintf ff "%a@ " Dom.fprint a
        with Not_found -> Format.fprintf ff "⊤@ " 
      in
      match s with
      | Ast.Asn _ | Ast.Asrt _ -> ()
      | Ast.Seq (_, s1, s2)
      | Ast.Ite (_, _, s1, s2) -> fprint_stm ff s1; fprint_stm ff s2
      | Ast.While (l, g, s) ->
        fprint_annot ff (Location.beg_p (Ast.loc_of_guard g)); fprint_stm ff s 
    in
    Utils.with_out_ch output_filename (fun out_ch ->
      let ff = Format.formatter_of_out_channel out_ch in
      Format.fprintf ff "@[<v>";
      fprint_stm ff t;
      Format.fprintf ff "@]%!");
    Report.nlogf 1 "Results written to %s."
      (Utils.output_filename_string output_filename)
end
