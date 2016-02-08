{
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

open Parser

exception Lexing_error of string

(* [q_of_string s1 s2] parses "s1.s2" as a Q.t. Q.of_string doesn't
   handle decimal notation with a dot, we have to do it ourselves. *)
let q_of_string s1 s2 = match s2 with
  | None -> Q.of_string s1
  | Some s2 ->
     let n1 = Q.of_string s1 in
     let n2 = Q.of_string s2 in
     let m =
       Q.of_string ("1" ^ String.make (String.length s2) '0') in
     Q.add n1 (Q.div n2 m)



}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let blank = [' ' '\r' '\t']
let nl = ['\r']?['\n']

rule token = parse
  | nl { Lexing.new_line lexbuf; token lexbuf }
  | blank+ { token lexbuf }
  | "/*" { comment lexbuf }
  | "++" { PLUS2 }
  | "--" { MINUS2 }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '(' { LPAR }
  | ')' { RPAR }
  | "rand" { RAND_ITV }
  | '=' { EQUAL }
  | ';' { SEMICOL }
  | ':' { DBLPOINT }
  | ',' { COMMA }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "int" { INTTYPE }
  | "real" { REALTYPE }
  | "bool" { BOOLTYPE }
  | '{' { LBRA }
  | '}' { RBRA }
  | '>' { GT }
  | '<' { LT }
  | ">=" { GE }
  | "<=" { LE }
  | ((('0' | (['1'-'9'] digit*)) as n1) ('.' ((digit*) as n2))?) as s { NUM (q_of_string n1 n2, s, Ast.RealT) }
  | '.' ((digit+) as n) as s { NUM (q_of_string "" (Some n), s, Ast.RealT) }
  | ('0' | (['1'-'9'] digit*)) as n
      { (* let n = *)
        (*   try int_of_string n *)
        (*   with Failure "int_of_string" -> *)
        (*     raise (Lexing_error "constant overflow") in *)
        NUM (Q.of_string n, n, Ast.IntT) }
  | (['_']* alpha (alpha|digit|['_'])*) as n { VAR n }
  | eof { EOF }
  | _ { raise (Lexing_error "unknown char") }

and comment = parse
  | nl { Lexing.new_line lexbuf; comment lexbuf }
  | "*/" { token lexbuf }
  | _ { comment lexbuf }
