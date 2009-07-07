(*
 * Stem is a functional, staticly-typed, concatenative language.
 * Copyright (C) 2009  Alex Leighton
 * This file is part of Stem
 *
 * Stem is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Stem is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Stem.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Contains all of the Stem ASL (abstract syntax list) types. *)

open Printf
open Lexing

(** A value of type [position] describes a section of text in a source file.
    [file] is the name of the file, [line] is the line number, [first] is
    the number of characters from the beginning of the line to the first
    character of the section, and [last] is the number of characters from
    the beginning of the line to the last character of the section. *)
type position = { file  : string; (* Name of the file. *)
                  line  : int;    (* Line number. *)
                  first : int;    (* Position of first character. *)
                  last  : int     (* Position of last character. *)
                }

let set_filename n p =
  { file = n; line = p.line; first = p.first; last = p.last; }

let string_of_position p = Printf.sprintf
  "File \"%s\", line %i, characters %i-%i" p.file p.line p.first p.last

let position_of_lexbuf lexbuf =
  let p = lexeme_start_p lexbuf in
  let endp  = lexeme_end lexbuf in
    { file  = p.pos_fname;
      line  = p.pos_lnum;
      first = p.pos_cnum - p.pos_bol;
      last  = endp - p.pos_cnum }

let merge_positions p p' =
  { file  = p.file;
    line  = p.line;
    first = (min p.first p'.first);
    last  = (max p.last  p'.last) }

type expr =
  | EInt of position * int
  | EFloat of position * float
  | EBool of position * bool
  | EString of position * string
  | EQuote of position * expr list
  | EWord of position * string
  | EPrimitive of position * string
  | EDefine of position * string * expr

let rec string_of_expr = function
  | EInt     (_,i)  -> (string_of_int i)
  | EFloat   (_,f)  -> sprintf "%f" f
  | EBool    (_,b)  -> if b then "true" else "false"
  | EString  (_,s)  -> "\"" ^ s ^ "\""
  | EQuote   (_,es) -> "["^(String.concat " " (List.map string_of_expr es))^"]"
  | EWord    (_,w)  -> w
  | EPrimitive (_,p)-> p
  | EDefine (_,w,e) -> sprintf "(%s : %s)" w (string_of_expr e)

let get_position = function
  | EInt       (p,_)   -> p
  | EFloat     (p,_)   -> p
  | EBool      (p,_)   -> p
  | EString    (p,_)   -> p
  | EQuote     (p,_)   -> p
  | EWord      (p,_)   -> p
  | EPrimitive (p,_)   -> p
  | EDefine    (p,_,_) -> p

let set_position p = function
  | EInt       (_,i)   -> EInt (p,i)
  | EFloat     (_,f)   -> EFloat (p,f)
  | EBool      (_,b)   -> EBool (p,b)
  | EString    (_,s)   -> EString (p,s)
  | EQuote     (_,es)  -> EQuote (p,es)
  | EWord      (_,w)   -> EWord (p,w)
  | EPrimitive (_,w)   -> EPrimitive (p,w)
  | EDefine    (_,w,e) -> EDefine (p,w,e)

let merge_expr_positions e e' =
  merge_positions (get_position e) (get_position e')

let merge_position_expr p e = merge_positions p (get_position e)

let set_filename_expr fn e =
  let p = set_filename fn (get_position e) in
    set_position p e
