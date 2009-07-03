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

(** Module for interacting with the Parrot vm via the Unix process creator. *)

(**/**)
let insert s ch = output_string ch s
let get_all = Filesystem.read_all
(**/**)

(** Creates a process that runs the Parrot VM with the given code. This
    function returns the standard-output of the process * standard-err of the
    process * the Unix.process_status given upon closing the process. It's
    clean too; any exceptions thrown will result in all channels being
    closed. *)
let run pir_code =
  let out_of_parrot,into_parrot,out_of_parrot_err =
    Unix.open_process_full "parrot -" (Unix.environment ())
  in
    try
      insert pir_code into_parrot;
      close_out into_parrot;
      let parrot_output = get_all out_of_parrot in
      let parrot_err    = get_all out_of_parrot_err in
      let status =
        Unix.close_process_full (out_of_parrot,into_parrot,out_of_parrot_err)
      in parrot_output,parrot_err,status
    with e ->
      ignore (Unix.close_process_full
                (out_of_parrot,into_parrot,out_of_parrot_err));
      raise e

let compile pir_code outfile =
  let parrot = sprintf "parrot -o %s --output-pbc -" outfile in
  let out_of_parrot,into_parrot,out_of_parrot_err =
    Unix.open_process_full parrot (Unix.environment ())
  in
    try
      insert pir_code into_parrot;
      close_out into_parrot;
      let parrot_output = get_all out_of_parrot in
      let parrot_err    = get_all out_of_parrot_err in
      let status =
        Unix.close_process_full (out_of_parrot,into_parrot,out_of_parrot_err)
      in parrot_output,parrot_err,status
    with e ->
      ignore (Unix.close_process_full
                (out_of_parrot,into_parrot,out_of_parrot_err));
      raise e
