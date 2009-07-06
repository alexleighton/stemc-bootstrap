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

(** Module exposing the Parrot string opcodes. *)

open Printf
open Ptypes

(**/**)
let spf = sprintf
(**/**)

let ord i0 s i1 =
  let i0, i1, s = get_int i0, get_int i1, get_str s in
    spf "%s = ord %s, %s" i0 s i1

let chr s i =
  let s, i = get_str s, get_int i in
    spf "%s = chr %s" s i

(** [concat a b c] concatenates [c] onto the end of [b] and stores the
    result in [a]. *)
let concat s0 s1 s2 =
  let s0, s1, s2 = get_str s0, get_str s1, get_str s2 in
    spf "%s = concat %s, %s" s0 s1 s2

(** Destructive version of {!concat}. Modifies the first string in place,
    appending the second string. *)
let concatD s0 s1 =
  let s0, s1 = get_str s0, get_str s1 in
    spf "%s .= %s" s0 s1

(** [repeat sout sin i] repeats [sin], the number of times stored in [i] and
    stores the resulting string in [sout]. *)
let repeat sout sin ireg =
  let sout, sin, ireg = get_str sout, get_str sin, get_int ireg in
    spf "%s = repeat %s, %s" sout sin ireg
