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

(** Parser module containing types and functions related to parsing. *)

open Lexing
open Stypes

(** A syntax error. Raised when either the lexer or parser has a syntax
    error. *)
exception Syntax_error of string

(** Raises a syntax error. Convenience function. *)
let syntax_err s = raise (Syntax_error s)

(** [eparse_aux lex fn] uses the expression parser to parse a given lexer
    buffer [lex] into a list of values. [fn] is the "filename" to be
    reported by the positions of all parsed expressions. *)
let eparse_aux lex fn =
  let cmds = try ExpressionParser.toplevel ExpressionLexer.token lex
  with
    | Failure ("lexing: empty token") ->
        let p = set_filename fn (position_of_lexbuf lex) in
          syntax_err (string_of_position p ^ ":\n Lexing Error.")
    | Parsing.Parse_error ->
        let p = set_filename fn (position_of_lexbuf lex) in
          syntax_err (string_of_position p ^ ":\n Syntax Error.")
  in
    List.map (set_filename_expr fn) cmds

(** [parse_expr str fn] uses the expression parser to parse a given string
    [str] into a list of values. [fn] is the "filename" to be reported by the
    positions of all parsed expressions. *)
let parse_expr str fn = eparse_aux (Lexing.from_string str) fn

(** [parse_expr_channel ch fn] uses the expression parser to parse an input
    channel [ch] into a list of values. [fn] is the "filename" to be
    reported by the positiions of all parsed expressions. *)
let parse_expr_channel ch fn = eparse_aux (Lexing.from_channel ch) fn
