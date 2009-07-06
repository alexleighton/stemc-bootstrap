open OUnit
open Pstring
open Ptypes
open Pcore

(** Module containing tests of the Pstring module. *)

(** Tests the PIR opcodes {!Pstring.ord} and {!Pstring.chr} for converting
    characters to their codepoints and back. *)
let test_ordchr _ =
  let alph = "abcdefghijklmnopqrstuvwxyz" in
  let i0,i1,s0,s1 = i 0,i 1,s 0,s 1 in
  let loopbody = String.concat "\n"
    [(ord i1 s0 i0); (chr s1 i1); (Pio.print_r s1)] in
  let body = String.concat "\n"
    [(set_s s0 alph); (forloop 0 26 i0 loopbody);] in
  let code = Psub.string_of_subroutine (Psub.main body) in
    Test_parrot.test_code code alph

(** Tests both {!Pstring.concat} and {!Pstring.concatD}. *)
let test_concat _ =
  let first,second = "hello ","world" in
  let s0,s1,s2 = s 0, s 1, s 2 in
  let body = String.concat "\n"
    [(set_s s0 first);
     (set_s s1 second);
     (concat s2 s0 s1);
     (concatD s0 s2);
     (Pio.print_r s0);] in
  let code = Psub.string_of_subroutine (Psub.main body) in
    Test_parrot.test_code code "hello hello world"

(** Tests {!Pstring.repeat}. *)
let test_repeat _ =
  let s0,i0 = s 0, i 0 in
  let body = String.concat "\n"
    [(set_s s0 "test");
     (set_i i0 3);
     (repeat s0 s0 i0);
     (Pio.print_r s0);] in
  let code = Psub.string_of_subroutine (Psub.main body) in
    Test_parrot.test_code code "testtesttest"

(** Tests {!Pstring.length}, the length op. *)
let test_length _ =
  let s0,s1,i0,i1 = s 0, s 1, i 0, i 1 in
  let body = String.concat "\n"
    [(set_s s0 ""); (set_s s1 "hello");
     (length i0 s0); (length i1 s1);
     (Pio.say_r i0); (Pio.say_r i1);] in
  let code = Psub.string_of_subroutine (Psub.main body) in
    Test_parrot.test_code code "0\n5"

(** Tests {!Pstring.sub}, the substring op. *)
let test_sub _ =
  let s0,s1,i0,i1 = s 0, s 1, i 0, i 1 in
  let body = String.concat "\n"
    [(set_s s0 "hello");
     (set_i i0 1);
     (sub s1 s0 i0);
     (Pio.say_r s1);
     (set_i i1 2);
     (sub s1 s0 i0 ~stop:i1);
     (Pio.say_r s1);] in
  let code = Psub.string_of_subroutine (Psub.main body) in
    Test_parrot.test_code code "ello\nel"

(** Pstring Test Suite *)
let pstring_suite = "Pstring Test Suite" >::: [
  "test_ordchr" >:: test_ordchr;
  "test_concat" >:: test_concat;
  "test_repeat" >:: test_repeat;
  "test_length" >:: test_length;
  "test_sub"    >:: test_sub;
]

let _ = run_test_tt ~verbose:!Test_parrot.verbosity pstring_suite
