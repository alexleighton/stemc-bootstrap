%{
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

  (** Auto-generated module for parsing the stem language. *)
  open Stypes

%}

%token <Stypes.position> LCBRACE RCBRACE LSBRACE RSBRACE LPAREN RPAREN
%token <Stypes.position*string> IDENT
%token <Stypes.position*int> INT
%token <Stypes.position*float> FLOAT
%token <Stypes.position> TRUE FALSE

%token <Stypes.position> DEFINE
%token <Stypes.position*string> STRING
%token <Stypes.position> PRIMITIVE
%token EOF

%start toplevel
%type <Stypes.expr list> toplevel

%%

toplevel:
  | exprs EOF               { $1 }

exprs:
  |                         { [] }              /* Thanks kaustuv. */
  | expr exprs              { $1 :: $2 }

expr:
  | literal                 { $1 }
  | word                    { $1 }
  | definition              { $1 }

literal:
  | INT                     { let p,i = $1 in EInt (p,i) }
  | FLOAT                   { let p,f = $1 in EFloat (p,f) }
  | STRING                  { let p,s = $1 in EString (p,s) }
  | TRUE                    { EBool ($1,true) }
  | FALSE                   { EBool ($1,false) }
  | quote                   { $1 }

word:
  | IDENT                   { let p,w = $1 in EWord (p,w) }

definition:
  | IDENT DEFINE literal    { let ip,name = $1 in
                              let p = merge_position_expr ip $3 in
                                EDefine (p,name,$3) }
  | IDENT DEFINE PRIMITIVE  { let (ip,name),pp = $1,$3 in
                              let p = merge_positions ip pp in
                                EDefine (p,name,EPrimitive(ip,name)) }

quote:
  | LSBRACE qexprs RSBRACE  { let lp,rp = $1,$3 in
                              let p = merge_positions lp rp in
                                EQuote (p,$2) }
  | LCBRACE qexprs RCBRACE  { let lp,rp = $1,$3 in
                              let p = merge_positions lp rp in
                                EQuote (p,$2) }

qexprs:
  |                         { [] }
  | qexpr qexprs            { $1 :: $2 }

qexpr:
  | literal                 { $1 }
  | word                    { $1 }

%%
