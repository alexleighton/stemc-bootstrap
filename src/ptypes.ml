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
type ptype = Int | Num | Str | PMC

(** A parrot register is a type and an integer. *)
type register = Reg of ptype * int | Local of ptype * string


type label = string

let spf = sprintf

let fail name msg = raise (Invalid_argument (spf "%s: %s" name msg))

(** [register r] converts a string into a register. The given string should
    be of the regexp form [inspINSP][0-9]+. *)
let register r =
  let len = String.length r in
    if len < 2 then fail "register" "The string is too short." else
      let first = r.[0] in
      let num = try int_of_string (String.sub r 1 (len-1))
      with Failure _ ->  fail "register" "The register index is not a number."
      in if num < 0 then fail "register" "The register index is invalid (<0)."
        else match first with
          | 'I' | 'i' -> Reg (Int,num)
          | 'N' | 'n' -> Reg (Num,num)
          | 'S' | 's' -> Reg (Str,num)
          | 'P' | 'p' -> Reg (PMC,num)
          | _ -> fail "register" "Invalid register type."

(** Alias of {!register}. *)
let reg = register

(** Converts a list of strings into a list of registers. *)
let registers = List.map register

(** Alias of {!register}. *)
let register_of_string = register

(** Returns a PIR-legal string representation of the register. *)
let string_of_register = function
  | Reg (Int,i)    -> sprintf "$I%i" i
  | Reg (Num,n)    -> sprintf "$N%i" n
  | Reg (Str,s)    -> sprintf "$S%i" s
  | Reg (PMC,p)    -> sprintf "$P%i" p
  | Local (_,name) -> name

(** Turns a parrot type into a string. *)
let string_of_ptype = function
  | Int -> "int" | Num -> "num" | Str -> "string"
  | PMC -> "pmc"

(** [get_int fn msg reg] returns the string representation of [reg].
    @raises Invalid_argument with the [fn] name and [msg] given when [reg]
    is not an int register. *)
let get_int name msg = function
  | Reg (Int,_) as i   -> string_of_register i
  | Local (Int,_) as i -> string_of_register i
  | _                  -> fail name msg

(** [get_num fn msg reg] returns the string representation of [reg].
    @raises Invalid_argument with the [fn] name and [msg] given when [reg]
    is not a num register. *)
let get_num name msg = function
  | Reg (Num,_) as n   -> string_of_register n
  | Local (Num,_) as n -> string_of_register n
  | _                  -> fail name msg

(** [get_str fn msg reg] returns the string representation of [reg].
    @raises Invalid_argument with the [fn] name and [msg] given when [reg]
    is not a string register. *)
let get_str name msg = function
  | Reg (Str,_) as s   -> string_of_register s
  | Local (Str,_) as s -> string_of_register s
  | _                  -> fail name msg

(** [get_pmc fn msg reg] returns the string representation of [reg].
    @raises Invalid_argument with the [fn] name and [msg] given when [reg]
    is not a pmc register. *)
let get_pmc name msg = function
  | Reg (PMC,_) as p   -> string_of_register p
  | Local (PMC,_) as p -> string_of_register p
  | _                  -> fail name msg

(** Returns the index associated with the given register.
    @raise Invalid_argument when given a local register. Local registers
    have no index. *)
let get_register_index = function
  | Reg (Int,i)    -> i
  | Reg (Num,n)    -> n
  | Reg (Str,s)    -> s
  | Reg (PMC,p)    -> p
  | Local _        ->
      fail "get_register_index" "Local registers have no index."

(** Returns the code form of a label, i.e. "<label>:" *)
let string_of_label l = sprintf "%s:" l

(**/**)
let label_num = ref 0
(**/**)

(** Generates a label that is guaranteed to be unique. *)
let unique_label () =
  let lbl = sprintf "label%i" !label_num in
    label_num := !label_num + 1;
    lbl
