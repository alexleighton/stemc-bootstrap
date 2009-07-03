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
open Ptypes

(** Module containing tests of the Ptypes module. *)

(**/**)
let spf = Printf.sprintf
(**/**)

(** Tests that the register function rejects strings that represent invalid
    registers. *)
let test_register_invalid _ =
  let eshortstr = (Invalid_argument "register: The string is too short.") in
  let eregbad = (Invalid_argument
                   "register: The register index is not a number.") in
  let ereginv = (Invalid_argument
                   "register: The register index is invalid (<0).") in
  let ebadtype = (Invalid_argument "register: Invalid register type.") in
  let register s = fun () -> register s in
    assert_raises ~msg:"Empty string not rejected." eshortstr (register "");
    assert_raises ereginv (register "i-1")
      ~msg:"Invalid index (i-1) not rejected." ;
    assert_raises ereginv (register "i-2000")
      ~msg:"Invalid index (i-2000) not rejected." ;
    assert_raises eregbad (register "ibad")
      ~msg:"Invalid index (ibad) not rejected." ;
    assert_raises eregbad (register "i2.3")
      ~msg:"Invalid index (i2.3) not rejected." ;
    assert_raises ebadtype (register "t2")
      ~msg:"Invalid type (t) not rejected."

(** Tests that the register datatype returned by the register function is
    what it should be. *)
let test_register_type _ =
  let p = fun x -> (string_of_register x) in
  let msg = "register does not convert strings properly: " in
  let m i = msg ^ " " ^ (string_of_int i) ^ "." in
  let assert_eq i r0 r1 = assert_equal ~printer:p ~msg:(m i) r0 r1 in
    for i = 0 to 1000 do
      assert_eq i (register (spf "i%i" i)) (Reg (Int,i));
      assert_eq i (register (spf "I%i" i)) (Reg (Int,i));
      assert_eq i (register (spf "n%i" i)) (Reg (Num,i));
      assert_eq i (register (spf "N%i" i)) (Reg (Num,i));
      assert_eq i (register (spf "s%i" i)) (Reg (Str,i));
      assert_eq i (register (spf "S%i" i)) (Reg (Str,i));
      assert_eq i (register (spf "p%i" i)) (Reg (PMC,i));
      assert_eq i (register (spf "P%i" i)) (Reg (PMC,i));
    done

(** Tests that a register representation makes it through several translations
    to remain a valid register in PIR code. *)
let test_register_valid _ =
  let idef = 0 in
  let ndef = 3.14 in
  let sdef = "hello" in
  let body = Buffer.create 20000 in
    for i = 0 to 100 do
      let i_n = register (spf "i%i" i) in
      let n_n = register (spf "n%i" i) in
      let s_n = register (spf "s%i" i) in
        Buffer.add_string body ((Pcore.set_i i_n idef) ^ "\n");
        Buffer.add_string body ((Pcore.set_n n_n ndef) ^ "\n");
        Buffer.add_string body ((Pcore.set_s s_n sdef) ^ "\n");
    done;
    Buffer.add_string body "say $S0";
    let main = Psub.main (Buffer.contents body) in
    let code = Psub.string_of_subroutine main in
      Test_parrot.test_code code "hello"

(** Ptypes Test Suite *)
let ptypes_suite = "Ptypes Test Suite" >::: [
  "test_register_invalid" >:: test_register_invalid;
  "test_register_type" >:: test_register_type;
  "test_register_valid" >:: test_register_valid;
]

let _ = run_test_tt ~verbose:!Test_parrot.verbosity ptypes_suite
