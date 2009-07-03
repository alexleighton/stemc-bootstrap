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

(** Contains convenience functions for reading and writing to file. *)


(** [read_all channel] returns all output from an output channel, including
    any newlines. *)
let read_all ch =
  let str = Buffer.create 1000 in
  let rec loop () = Buffer.add_string str (input_line ch ^ "\n"); loop () in
    try ignore (loop ()); assert false
    with End_of_file ->
      let len = Buffer.length str in
        if len > 0 then Buffer.sub str 0 (len-1) else ""

(** [write_all channel text] writes all of the [text] to the channel. *)
let write_all ch text = output_string ch text; flush ch

(** [with_open_file file f] opens [file] and gives the out_channel to [f].
    The out_channel opened for the file clears the file of any text and is
    set to write mode. The out_channel associated with [file] is closed after
    the function returns, or in the event of an exception.
    @raise Sys_error if the file could not be opened. *)
let with_write_file file f =
  let file_channel = open_out file in
    try f file_channel; close_out file_channel
    with e -> close_out file_channel; raise e

(** [with_read_file file f] opens [file] and gives the in_channel to [f].
    The in_channel opened for the file is set to read mode. The in_channel
    associated with [file] is closed after the function returns, or in the
    event of an exception.
    @raise Sys_error if the file could not be opened. *)
let with_read_file file f =
  let file_channel = open_in file in
    try f file_channel; close_in file_channel
    with e -> close_in file_channel; raise e

(** [write_to_file file text] writes [text] to [file], overwriting the
    previous contents of the file, handling the closing of the file.
    @raise Sys_error if the file could not be opened. Like if it does not
    exist. *)
let write_to_file file text =
  with_write_file file (fun ch -> output_string ch text)

(** [read_file file] reads all of the text from a file into a string.
    @raise Sys_error if the file could not be opened. Like if it does not
    exist. *)
let read_file file = let text = ref "" in
  with_read_file file (fun ch -> text := read_all ch); !text
