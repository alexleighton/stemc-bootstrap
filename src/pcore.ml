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

let sor = string_of_register

let set r0 r1 =
  sprintf "set %s, %s" (sor r0) (sor r1)

let set_s r0 s =
  sprintf "set %s, '%s'" (sor r0) s

let set_i r0 i =
  sprintf "set %s, %i" (sor r0) i

let set_n r0 n =
  sprintf "set %s, %f" (sor r0) n

let clone p0 p1 =
  let p0 = get_pmc "clone" "A PMC register is required first." p0 in
  let p1 = get_pmc "clone" "A PMC register is required second." p1 in
    sprintf "clone %s, %s" p1 p0

let swap r0 r1 =
  sprintf "exchange %s, %s" (sor r0) (sor r1)
