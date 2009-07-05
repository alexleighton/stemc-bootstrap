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

(** Module that makes creation and interaction with the Array PMC easy. *)

(**/**)
let spf = sprintf
(**/**)

(** [get_size p i] sets the int register [i] to the size of the array in
    the PMC register [p]. *)
let get_size p i =
  let p, i = get_pmc p, get_int i in
    spf "\nset %s, %s" i p

(** [set_size p size] sets the size of the array in register [p] to the
    integer literal [size]. *)
let set_size p size =
  let p = get_pmc p in
    spf "\nset %s, %i" p size

(** [set_size_r p i] is like {!set_size} except the size of the array is set
    to the integer contained in the int register [i]. *)
let set_size_r p i =
  let p, i = get_pmc p, get_int i in
    spf "\nset %s, %s" p i

(** [make_array p] creates a new array of size 0 in the PMC register [p].
    An integer literal size can be specified optionally. *)
let make_array ?(size= -1) p =
  let preg = get_pmc p in
    spf "\nnew %s, 'Array'%s" preg (if size= -1 then "" else set_size p size)

(** [get p i o] is [o = p\[i\]]. Gets the value stored at location [i] (an
    integer literal) in the array in [p] (a PMC register) and puts it into
    the register [o]. *)
let get p i o =
  let p = get_pmc p in
    spf "%s = %s[%i]" (string_of_register o) p i

(** [get_r p i o] is [o = p\[i\]]. Gets the value stored at location [i] (an
    int register) in the array in [p] (a PMC register) and puts it into the
    register [o]. *)
let get_r p i o =
  let p, i = get_pmc p, get_int i in
    spf "%s = %s[%s]" (string_of_register o) p i

(** [set p i o] is [p\[i\] = o]. Sets the value stored at location [i] (an
    integer literal) in the array in [p] (a PMC register) to the value in
    register [o]. *)
let set p i o =
  let p = get_pmc p in
    spf "%s[%i] = %s" p i (string_of_register o)

(** [set_r p i o] is [p\[i\] = o]. Sets the value stored at location [i] (an
    int register) in the array in [p] (a PMC register) to the value in
    register [o]. *)
let set_r p i o =
  let p, i = get_pmc p, get_int i in
    spf "%s[%s] = %s" p i (string_of_register o)
