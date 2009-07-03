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

open Pcore
open Psub
open Pio
open Ptypes
open OUnit

(** Module containing tests of the Pcore module. *)

(** Tests setting of the various parrot registers. *)
let test_set _ =
  let i0,i1,i2 = reg "I0", reg "I1", reg "I2" in
  let n0,n1,n2 = reg "N0", reg "N1", reg "N2" in
  let s0,s1,s2 = reg "S0", reg "S1", reg "S2" in
  let body = String.concat "\n"
    [(set_s s0 "hello");
     (set_s s1 "world");
     (set_i i0 5);
     (set_i i1 10);
     (set_n n0 2.5);
     (set_n n1 3.5);
     (print_r s0); (print " "); (print_r s1); (print " ");
     (print_r i0); (print " "); (print_r i1); (print " ");
     (print_r n0); (print " "); (print_r n1);] in
  let main_sub = main body in
  let code = string_of_subroutine main_sub in
    Test_parrot.test_code code "hello world 5 10 2.5 3.5"

(** Tests the clone opcode for cloning PMC objects. *)
let test_clone _ =
  let p0, p1 = reg "P0", reg "P1" in
  let body = String.concat "\n"
    ["$P0 = new 'String'";
     "$P1 = new 'String'";
     "$P0 = 'hello'";
     (clone p0 p1);
     (print_r p1)] in
  let main_sub = main body in
  let code = string_of_subroutine main_sub in
    Test_parrot.test_code code "hello"

(** Tests the swap opcode for swapping two registers. *)
let test_swap _ =
  let i0, i1 = reg "I0", reg "I1" in
  let n0, n1 = reg "N0", reg "N1" in
  let s0, s1 = reg "S0", reg "S1" in
  let body = String.concat "\n"
    [(set_i i0 5);       (set_i i1 10);
     (set_n n0 3.14);    (set_n n1 2.7);
     (set_s s0 "hello"); (set_s s1 "world");
     (swap i0 i1); (swap n0 n1); (swap s0 s1);
     (print_r i1); (print " ");
     (print_r n1); (print " ");
     (print_r s1);] in
  let main_sub = main body in
  let code = string_of_subroutine main_sub in
    Test_parrot.test_code code "5 3.14 hello"

(** Tests the forloop code for creating a simple for loop. *)
let test_forloop _ =
  let output = "0123456789 9876543210" in
  let i0, i1 = reg "i0", reg "i1" in
  let body = String.concat "\n"
    [(forloop 0 10 i0 (print_r i0));
     (print " ");
     (forloop 9 (-1) i0 (print_r i0) ~down:true);] in
  let code = string_of_subroutine (main body) in
    Test_parrot.test_code code output

(** Core Test Suite *)
let core_suite = "PIR Core Code Gen Suite" >::: [
  "test_set"     >:: test_set;
  "test_clone"   >:: test_clone;
  "test_swap"    >:: test_swap;
  "test_forloop" >:: test_forloop;
]

let _ = run_test_tt ~verbose:!Test_parrot.verbosity core_suite
