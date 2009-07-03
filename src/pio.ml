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
  | Some p -> let p = get_pmc "say" "A PMC register is required." p in
      sprintf "say %s, '%s'" p s

let say_r reg = sprintf "say %s" (string_of_register reg)

let print ?(out) s = match out with
  | None   -> sprintf "print '%s'" s
  | Some p -> let p = get_pmc "print" "A PMC register is required." p in
      sprintf "print %s, %s" p s

let print_r ?(out) s = match out with
  | None   -> sprintf "print %s" (string_of_register s)
  | Some p -> let p = get_pmc "print_r" "A PMC register is required." p in
      sprintf "print %s, %s" p (string_of_register s)

(* Valid modes are "r", "w", "wa", "p" *)
let open_file reg ?(mode="r") filename =
  let p = get_pmc "open_file" "A PMC register is required." reg in
    sprintf "%s = open '%s', '%s'" p filename mode

(* Valid modes are "r", "w", "wa", "p" *)
let open_file_r p ?(mode="r") s =
  let p = get_pmc "open_file_r" "A PMC register is required first." p in
  let s = get_str "open_file_r" "A string register is required second." s in
    sprintf "%s = open %s, '%s'" p s mode

let getstdin reg =
  let p = get_pmc "getstdin" "A PMC register is required." reg in
    sprintf "%s = getstdin" p

let getstdout reg =
  let p = get_pmc "getstdout" "A PMC register is required." reg in
    sprintf "%s = getstdout" p

let getstderr reg =
  let p = get_pmc "getstdout" "A PMC register is required." reg in
    sprintf "%s = getstderr" p

let close reg =
  let p = get_pmc "close" "A PMC register is required." reg in
    sprintf "close %s" p

let is_closed i p =
  let i = get_int "is_closed" "An integer register is required first." i in
  let p = get_pmc "is_closed" "A PMC register is required second." p in
    sprintf "%s = %s.'is_closed'()" i p

let read s p i =
  let s = get_str "read" "A string register is required first." s in
  let p = get_pmc "read" "A PMC register is required second." p in
    sprintf "%s = read %s, %i" s p i

let read_r s p i =
  let s = get_str "read_r" "A string register is required first." s in
  let p = get_pmc "read_r" "A PMC register is required second." p in
  let i = get_int "read_r" "An integer register is required third." i in
    sprintf "%s = read %s, %s" s p i

let readline p s =
  let p = get_pmc "readline" "A PMC register is required first." p in
  let s = get_str "readline" "A string register is required second." s in
    sprintf "%s = readline %s" s p

let readall s0 p s1 =
  let s0 = get_str "readall" "A string register is required first." s0 in
  let p = get_pmc "readall" "A PMC registe is required second." p in
    sprintf "%s = new 'FileHandle'\n%s = %s.'readall'('%s')" p s0 p s1

let readall_r s0 p s1 =
  let s0 = get_str "readall_r" "A string register is required first." s0 in
  let p = get_pmc "readall_r" "A PMC register is required second." p in
  let s1 = get_str "readall_r" "A string register is required third." s1 in
    sprintf "%s = %s.'readall'(%s)" s0 p s1
