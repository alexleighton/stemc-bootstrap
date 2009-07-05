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

open Printf
open Ptypes

let say ?(out) s = match out with
  | None   -> sprintf "say '%s'" s
  | Some p -> sprintf "say %s, '%s'" (get_pmc p) s

let say_r reg = sprintf "say %s" (string_of_register reg)

let print ?(out) s = match out with
  | None   -> sprintf "print '%s'" s
  | Some p -> sprintf "print %s, %s" (get_pmc p) s

let print_r ?(out) s = match out with
  | None   -> sprintf "print %s" (string_of_register s)
  | Some p -> sprintf "print %s, %s" (get_pmc p) (string_of_register s)

(* Valid modes are "r", "w", "wa", "p" *)
let open_file reg ?(mode="r") filename =
    sprintf "%s = open '%s', '%s'" (get_pmc reg) filename mode

(* Valid modes are "r", "w", "wa", "p" *)
let open_file_r p ?(mode="r") s =
  let p, s = get_pmc p, get_str s in
    sprintf "%s = open %s, '%s'" p s mode

let getstdin p = sprintf "%s = getstdin" (get_pmc p)

let getstdout p = sprintf "%s = getstdout" (get_pmc p)

let getstderr p = sprintf "%s = getstderr" (get_pmc p)

let close p = sprintf "close %s" (get_pmc p)

let is_closed i p = sprintf "%s = %s.'is_closed'()" (get_int i) (get_pmc p)

let read s p i = sprintf "%s = read %s, %i" (get_str s) (get_pmc p) i

let read_r s p i =
  let s, p, i = get_str s, get_pmc p, get_int i in
    sprintf "%s = read %s, %s" s p i

let readline p s = sprintf "%s = readline %s" (get_str s) (get_pmc p)

let readall s0 p s1 =
  let s0, p = get_str s0, get_pmc p in
    sprintf "%s = new 'FileHandle'\n%s = %s.'readall'('%s')" p s0 p s1

let readall_r s0 p s1 =
  let s0, p, s1 = get_str s0, get_pmc p, get_str s1 in
    sprintf "%s = %s.'readall'(%s)" s0 p s1
