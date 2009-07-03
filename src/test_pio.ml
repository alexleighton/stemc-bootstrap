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
open Pio
open Ptypes

(** Module containing tests of the Pio module. *)

(**/**)
let with_open_file = Filesystem.with_write_file
(**/**)

let temp_file = "temp.txt"
let temp_contents = "hello world\ntesting\ntesting"
let fill_temp_file ch = output_string ch temp_contents; close_out ch
let delete_temp_file () = Sys.remove temp_file

(** Tests the printing opcodes by printing to stdout. *)
let test_stdout_printing _ =
  let s0 = reg "s0" in
  let body = String.concat "\n"
    [(say "hello"); "$S0 = 'world'"; (say_r s0);
     (print "hello"); (print " "); (print_r s0);] in
  let main = Psub.main body in
  let code = Psub.string_of_subroutine main in
    Test_parrot.test_code code "hello\nworld\nhello world"

(** Tests that files can be created in Parrot using the given opcodes. *)
let test_file_creation _ =
  let s0,p0 = reg "s0", reg "p0" in
  let body = String.concat "\n"
    [(open_file p0 ~mode:"w" temp_file);
     (open_file p0 ~mode:"r" temp_file);
     (Pcore.set_s s0 temp_file);
     (open_file_r p0 ~mode:"wa" s0);
     (open_file_r p0 ~mode:"r" s0);
     (say "hello");] in
  let code = Psub.string_of_subroutine (Psub.main body) in
    Test_parrot.test_code code "hello"

(** Tests the PIR readall opcode. Creates a file in ocaml then attempts to
    read the file in parrot and then checks the input against the output. *)
let test_file_readall _ =
  with_open_file temp_file fill_temp_file;
  let p0 = reg "p0" in
  let s0 = reg "s0" in
  let body = String.concat "\n"
    [(readall s0 p0 temp_file);
     (say_r s0)] in
  let code = Psub.string_of_subroutine (Psub.main body) in
    Test_parrot.test_code code temp_contents;
    delete_temp_file ()

(** Reads all the lines from a file using readline, comparing the output
    with what was put into the file. *)
let test_file_read _ =
  with_open_file temp_file fill_temp_file;
  let p0 = reg "p0" in
  let s0 = reg "s0" in
  let body = String.concat "\n"
    [(open_file p0 temp_file);
     (readline p0 s0);
     (print_r s0);
     (readline p0 s0);
     (print_r s0);
     (readline p0 s0);
     (print_r s0)] in
  let code = Psub.string_of_subroutine (Psub.main body) in
    Test_parrot.test_code code temp_contents;
    delete_temp_file ()

(** Tests the close and is_closed functions. *)
let test_file_closing _ =
  with_open_file temp_file (fun ch -> ());
  let i0,p0 = reg "i0", reg "p0" in
  let body = String.concat "\n"
    [(open_file p0 temp_file);
     (close p0);
     (is_closed i0 p0);
     "$I0 = !$I0";
     "if $I0 goto not_closed";
     "say 'file closed'";
     "not_closed:"] in
  let code = Psub.string_of_subroutine (Psub.main body) in
    Test_parrot.test_code code "file closed";
    delete_temp_file ()

(** Parrot IO Test Suite *)
let pio_suite = "Parrot IO Test Suite" >::: [
  "test_stdout_printing" >:: test_stdout_printing;
  "test_file_creation"   >:: test_file_creation;
  "test_file_readall"    >:: test_file_readall;
  "test_file_read"       >:: test_file_read;
  "test_file_closing"    >:: test_file_closing;
]

let _ = run_test_tt ~verbose:!Test_parrot.verbosity pio_suite
