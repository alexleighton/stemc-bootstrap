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
open Ptypes

type buffer = Buffer.t

type parameter =
  | Regular of ptype * string
  | Named of ptype * string * string
  | Optional of ptype * string

type subroutine = {
  mutable name : string;
  mutable is_main : bool;
  mutable params : parameter list;
  mutable locals : register list;
  mutable body : string
}

let make_spaces i =
  if i < 0 then invalid_arg "make_spaces: given int must be > 0."
  else let result = ref "" in
    for j = 1 to i do result := !result ^ " " done; !result

let tab s i =
  let spaces = make_spaces i in
    Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) s

(* Make params does not touch the order of the parameters except to move all
   named parameters to the end of the list. For more info see the Parrot Trac
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

let make_locals = List.fold_left
  (fun s -> function
     | Local (typ,name) -> s ^
         (sprintf "\n.local %s %s" (string_of_ptype typ) name)
     | _ -> s) ""

let string_of_subroutine s =
  let tab_width = 4 in
  let str =
    sprintf ".sub '%s' %s%s%s\n    %s\n.end" s.name
      (if s.is_main then ":main" else "")
      (tab (make_locals s.locals) tab_width)
      (tab (make_params s.params) tab_width)
      (tab s.body tab_width)
  in
    str

let main ?(name="main") ?(params=[]) ?(locals=[]) body =
  { name = name; is_main = true; params = params; locals=locals; body = body }
