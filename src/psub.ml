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

(** Subroutine PIR module. *)

open Printf
open Ptypes

(** The type of a parameter. Both regular and optional parameters are
    positional. This means order matters when calling the subrouting.
    Named parameters are not positional. When usinga named parameter, it must
    be put at the end, after all positional parameters. *)
type parameter =
  | Regular of ptype * string         (* A regular positional parameter. *)
  | Optional of ptype * string        (* An optional positional parameter. *)
  | Named of ptype * string * string  (* A named parameter. *)

(** The type of a subroutine. A subroutine has a [name], knows whether or not
    it [is_main], a list of [params], a list of local registers to be
    initialized at the beginning of the subroutine, and a body of code. *)
type subroutine = {
  mutable name : string;           (* The name of the subroutine. *)
  mutable is_main : bool;          (* Is this the main subroutine. *)
  mutable params : parameter list; (* List of parameters it takes. *)
  mutable locals : register list;  (* List of local registers to init. *)
  mutable body : string            (* Body of code. *)
}

(** [make_spaces i] returns a string containing [i] number of spaces. *)
let make_spaces i =
  if i < 0 then invalid_arg "make_spaces: given int must be > 0."
  else let result = ref "" in
    for j = 1 to i do result := !result ^ " " done; !result

(** [tab s i] returns [s] with [i] number of spaces inserted after every
    newline. *)
let tab s i =
  let spaces = make_spaces i in
    Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) s

(** [make_params ps] turns all given parameters [ps] into legitimate PIR code.
    It does not touch the order of the parameters except to move all named
    parameters to the end of the list. For more info see the Parrot Trac
    ticket <https://trac.parrot.org/parrot/ticket/792> *)
let make_params all_params =
  let convert = List.fold_left
    (fun s p -> match p with
       | Regular (typ,name) -> s ^
           (sprintf "\n.param %s %s" (string_of_ptype typ) name)
       | Named (typ,name,oname) -> s ^
           (sprintf "\n.param %s %s :named(\"%s\")"
              (string_of_ptype typ) name oname)
       | Optional (typ,name) -> s ^
           (sprintf "\n.param %s %s :optional\n.param int has_%s :opt_flag"
              (string_of_ptype typ) name name)
    ) "" in
  let pos_params,named_params =
    List.partition (function | Named _ -> false | _ -> true) all_params
  in
    (convert pos_params) ^ (convert named_params)

(** [make_locals ls] turns all given local registers [ls] into legitimate
    PIR code to initialize them. Does not touch the order. *)
let make_locals = List.fold_left
  (fun s -> function
     | `Local (typ,name) -> s ^
         (sprintf "\n.local %s %s" (string_of_ptype typ) name)
     | _ -> s) ""

(** Given a subroutine, this function turns it into functioning PIR code.
    Optionally specify the number of spaces to indent body code. *)
let string_of_subroutine ?(tabwidth=4) s =
  let str =
    sprintf ".sub '%s' %s%s%s\n    %s\n.end" s.name
      (if s.is_main then ":main" else "")
      (tab (make_locals s.locals) tabwidth)
      (tab (make_params s.params) tabwidth)
      (tab s.body tabwidth)
  in
    str

(** Returns a subroutine with the given body with the [is_main] flag set. *)
let main ?(name="main") ?(params=[]) ?(locals=[]) body =
  { name = name; is_main = true; params = params; locals=locals; body = body }
