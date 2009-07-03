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

open Parray
open Ptypes
open Pio
open Psub
open Printf
open OUnit

(** Module containing tests of the Parray module. *)

(** Test creation of an array PMC. *)
let test_array_creation _ =
  let p0,p1 = reg "P0", reg "P1" in
  let i0 = reg "I0" in
  let body = String.concat "\n"
    [(make_array p0 ~size:10);
     (make_array p1);
     (get_size p0 i0); (print_r i0); (print " ");
     (get_size p1 i0); (say_r i0);] in
  let main_sub = main body in
  let code = string_of_subroutine main_sub in
    Test_parrot.test_code code "10 0"

(** Test array PMC sizing. Specifically setting the size and then getting the
    size of arrays. *)
let test_array_sizing _ =
  let p0 = reg "P0" in
  let i0 = reg "I0" in
  let body = String.concat "\n"
    [(make_array p0);
     (get_size p0 i0);
     (print_r i0);
     (print " ");
     (set_size p0 60);
     (get_size p0 i0);
     (say_r i0)] in
  let main_sub = main body in
  let code = string_of_subroutine main_sub in
    Test_parrot.test_code code "0 60"

(** Test getting and setting of values in an array PMC. *)
let test_array_getters_setters _ =
  let p0 = reg "P0" in
  let i0 = reg "I0" in
  let i1 = reg "I1" in
  let body = String.concat "\n"
    [(make_array p0);
     (set_size p0 10);
     "$I0 = 12";
     (set p0 0 i0);
     "$I0 = 15";
     "$I1 = 1";
     (set_r p0 i1 i0);
     (get p0 0 i0);
     (print_r i0);
     (print " ");
     "$I1 = 1";
     (get_r p0 i1 i0);
     (print_r i0)] in
  let main_sub = main body in
  let code = string_of_subroutine main_sub in
    Test_parrot.test_code code "12 15"

(** Array Test Suite *)
let array_suite = "PIR Array Code Gen Suite" >::: [
  "test_array_creation" >:: test_array_creation;
  "test_array_sizing" >:: test_array_sizing;
  "test_array_getters_setters" >:: test_array_getters_setters;
]

let _ = run_test_tt ~verbose:!Test_parrot.verbosity array_suite
