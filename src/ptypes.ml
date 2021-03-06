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

(** Module containing OCaml types representing various aspects of the Parrot
    PIR language. The types contained in this module are used throughout
    the other modules used to write PIR code. *)

open Printf

(** Type representing the 4 types in parrot. *)
type ptype = [`Int | `Num | `Str | `PMC]

(** A parrot register is a ptype and an integer. OR it is a local register
    of a ptype and a name. *)
type register =
    [`Int of int | `Num of int | `Str of int | `PMC of int |
     `Local of ptype * string]

(** Integer register type. *)
type intreg = [`Int of int | `Local of [`Int] * string]

(** Number register type. *)
type numreg = [`Num of int | `Local of [`Num] * string]

(** String register type. *)
type strreg = [`Str of int | `Local of [`Str] * string]

(** PMC register type. *)
type pmcreg = [`PMC of int | `Local of [`PMC] * string]

(**/**)
let spf = sprintf
(**/**)

let p : int -> pmcreg = fun i -> `PMC(i)
let i : int -> intreg = fun i -> `Int(i)
let n : int -> numreg = fun i -> `Num(i)
let s : int -> strreg = fun i -> `Str(i)

(** Returns a PIR-legal string representation of the register. *)
let string_of_register = function
  | `Int(i)         -> sprintf "$I%i" i
  | `Num(n)         -> sprintf "$N%i" n
  | `Str(s)         -> sprintf "$S%i" s
  | `PMC(p)         -> sprintf "$P%i" p
  | `Local (_,name) -> name

(** Turns a parrot type into a string. *)
let string_of_ptype : ptype -> string = function
  | `Int -> "int" | `Num -> "num" | `Str -> "string"
  | `PMC -> "pmc"

(** [get_int fn msg reg] returns the string representation of [reg]. *)
let get_int : intreg -> string = function
  | `Int(_) as i          -> string_of_register i
  | `Local(`Int,_) as i   -> string_of_register i

(** [get_num fn msg reg] returns the string representation of [reg]. *)
let get_num : numreg -> string = function
  | `Num(_) as n        -> string_of_register n
  | `Local(`Num,_) as n -> string_of_register n

(** [get_str fn msg reg] returns the string representation of [reg]. *)
let get_str : strreg -> string = function
  | `Str(_) as s        -> string_of_register s
  | `Local(`Str,_) as s -> string_of_register s

(** [get_pmc fn msg reg] returns the string representation of [reg]. *)
let get_pmc : pmcreg -> string = function
  | `PMC(_) as p        -> string_of_register p
  | `Local(`PMC,_) as p -> string_of_register p

(** Returns the index associated with the given register. *)
let get_register_index = function
  | `Int(i)    -> i
  | `Num(n)    -> n
  | `Str(s)    -> s
  | `PMC(p)    -> p

(**/**)
let label_num = ref 0
(**/**)

(** Generates a label that is guaranteed to be unique. *)
let unique_label = fun () ->
  let lbl = sprintf "label%i" !label_num in
    label_num := !label_num + 1;
    lbl
