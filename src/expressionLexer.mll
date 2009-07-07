{
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

  (** Auto-generated module lexing a given string into tokens suitable for the
      auto-generated parser. *)

  open Lexing
  open ExpressionParser

  (** Increments the current line number in the lexer buffer. *)
  let inc_linenum lexbuff =
    let pos = lexbuff.lex_curr_p in
      lexbuff.lex_curr_p <-
        { pos with
            pos_lnum = pos.pos_lnum + 1;
            pos_bol  = pos.pos_cnum;
        }

  let get_pos = Stypes.position_of_lexbuf

}

let ident =
  [':''=''<''>''_''+''-''*''/''a'-'z' 'A'-'Z' '0'-'9']
  [':''=''<''>''_''+''-''*''/''a'-'z' 'A'-'Z' '0'-'9' '?']*

let float_number = '-'?['0'-'9']+('.'['0'-'9']*)? | '-'?'.'['0'-'9']+

rule token = parse
  | "#" [^'\n']* '\n'   { inc_linenum lexbuf; token lexbuf }
  | ";" [^'\n']* '\n'   { inc_linenum lexbuf; token lexbuf }
  | '\n'                { inc_linenum lexbuf; token lexbuf }
  | [' ' '\t']          { token lexbuf }
  | '-'?['0'-'9']+ as i { INT (get_pos lexbuf, (int_of_string i)) }
  | float_number as f   { FLOAT (get_pos lexbuf, (float_of_string f)) }
  | "true"              { TRUE (get_pos lexbuf) }
  | "false"             { FALSE (get_pos lexbuf) }
  | ":"                 { DEFINE (get_pos lexbuf) }
  | "_primitive_"       { PRIMITIVE (get_pos lexbuf) }
  | '\"' [^'\"']* '\"'  { let str = lexeme lexbuf in
                          let s = String.sub str 1 (String.length str - 2) in
                            STRING (get_pos lexbuf, s) }
  | '\'' [^'\'']* '\''  { let str = lexeme lexbuf in
                          let s = String.sub str 1 (String.length str - 2) in
                            STRING (get_pos lexbuf, s) }
  | '('                 { LPAREN (get_pos lexbuf) }
  | ')'                 { RPAREN (get_pos lexbuf) }
  | '{'                 { LCBRACE (get_pos lexbuf) }
  | '}'                 { RCBRACE (get_pos lexbuf) }
  | '['                 { LSBRACE (get_pos lexbuf) }
  | ']'                 { RSBRACE (get_pos lexbuf) }
  | ident as id         { IDENT (get_pos lexbuf, id) }
  | eof                 { EOF }

{}
