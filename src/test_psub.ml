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

open OUnit
open Psub
open Ptypes

(** Module containing tests of the Psub module. *)

(** Tests {!Psub.make_spaces}. *)
let test_make_spaces _ =
  let msg = "make_spaces didn't make the correct amount of spaces." in
  let p = fun x -> x in
    assert_equal ~msg:msg ~printer:p (make_spaces 0) "";
    assert_equal ~msg:msg ~printer:p (make_spaces 1) " ";
    assert_equal ~msg:msg ~printer:p (make_spaces 2) "  ";
    assert_equal ~msg:msg ~printer:p (make_spaces 4) "    ";
    assert_equal ~msg:msg ~printer:p (make_spaces 10) "          ";
    assert_raises (Invalid_argument "make_spaces: given int must be > 0.")
      (fun () -> (make_spaces (-1)))
      ~msg:"make_spaces should reject integers less than 0."

(** Tests {!Psub.tab}. *)
let test_tabulate _ =
  let msg = "tab did not add tabs correctly." in
  let p = fun x -> x in
    assert_equal ~msg:msg ~printer:p "a\n  b\n  c\n  d" (tab "a\nb\nc\nd" 2);
    assert_equal ~msg:msg ~printer:p "\n a\n   b\n c" (tab "\na\n  b\nc" 1);
    assert_equal ~msg:msg ~printer:p "\na\n  b\nc" (tab "\na\n  b\nc" 0)

(** Tests creation of a subroutine with each of the different paramater
    types. *)
let test_code_soundness _ =
  let aux = { name = "aux"; is_main = false;
              body = "say a\nsay b\nsay c";
              params = [(Regular (Int,"a"));
                        (Named (Num,"b","b"));
                        (Optional (Str,"c"))];
              locals = []} in
  let main = main "'aux'(5, 'hello', \"b\" => 3.14)" in
  let code = (string_of_subroutine main)^"\n\n"^(string_of_subroutine aux) in
    Test_parrot.test_code code "5\n3.14\nhello"

(** Subroutine Test Suite *)
let psub_suite = "PIR Subroutine Test Suite" >::: [
  "test_make_spaces" >:: test_make_spaces;
  "test_tabulate" >:: test_tabulate;
  "test_code_soundness" >:: test_code_soundness;
]

let _ = run_test_tt ~verbose:!Test_parrot.verbosity psub_suite
