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

(** [forloop start end ireg body] returns a for loop starting at [start]
    and ending at [end], where the [ireg] register is incremented each
    iteration. [body] becomes the body of the for loop and is executed each
    iteration. Optionally, the for loop can be told to count down instead of
    up. *)
let forloop ?(down=false) s e ir body =
  if s < e && down then invalid_arg "forloop: start < end" else ();
  if s > e && not down then invalid_arg "forloop: start > end" else ();
  let l = unique_label () in
  let ir = get_int "forloop" "An int register is required first." ir in
  let init = sprintf "%s = %i\n%s:\n" ir s l in
  let rest = sprintf "%s\n%s\nif %s != %i goto %s" body
    (if down then "dec "^ir else "inc "^ir) ir e l
  in
    init ^ rest
