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
open Unix
open OUnit

(** Provides a test harness for tests involving parrot PIR code. *)

(**/**)
let spf = sprintf
(**/**)

(** Tests the given code against the expected output. It creates a process and
    runs Parrot, passing in the given code. It then checks the output with
    the expected output and makes OUnit assertions. *)
let test_code code expected_output =
  let parrot_output,parrot_err,process_status = Parrot.run code in
  let check_output =
    if parrot_output = "" then parrot_err else parrot_output
  in
    assert_equal expected_output check_output ~printer:(fun x->x)
      ~msg:("Output from parrot was not what was expected. Given code: \n"
            ^code);
    (match process_status with
       | WEXITED i -> if i != 0
         then assert_failure (spf "Parrot ended with exit code %i." i)
       | WSIGNALED i ->
           assert_failure (spf "Parrot was killed by signal number %i." i)
       | WSTOPPED i ->
           assert_failure (spf "Parrot was stopped by signal number %i." i))

(** {2 Tests for the Test Harness} *)

(** Tests the {!Parrot.get_all} function. *)
let test_parrot_get_all _ =
  let code0 = ".sub 'main' :main\nsay 'hello world'\n.end" in
  let code1 = ".sub 'main' :main\nsay 'testing. testing.'\nsay '123'\n.end" in
  let parrot_output,_,_ = Parrot.run code0 in
    assert_equal "hello world" parrot_output ~printer:(fun x->x)
      ~msg:"get_all function doesn't get everything.";
  let parrot_output,_,_ = Parrot.run code1 in
    assert_equal "testing. testing.\n123" parrot_output ~printer:(fun x->x)
      ~msg:"get_all function doesn't get everything."

(** Parrot Interface Test Suite *)
let parrot_test_suite = "Parrot Interface Suite"
  >::: ["test_parrot_get_all" >:: test_parrot_get_all]

(** Whether the test framework's verbosity is on or off. Passing a -verbose
    or -v argument on the commandline will result in [verbosity] being true.
    Defaults to false. *)
let verbosity = ref false

let _ =
  let spec = Arg.align [
    "-verbose",(Arg.Set verbosity)," sets the test output to verbose." ;
    "-v",(Arg.Set verbosity)," sets the test output to verbose." ] in
    Arg.parse spec (fun _ -> ()) "This program tests the stem compiler."

let _ = run_test_tt ~verbose:!verbosity parrot_test_suite
