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

(** A namespace module. *)

(** The type of a namespace. *)
type 'a t

(** Creates a new namespace with no bindings. *)
val create : unit -> 'a t

(** [is_empty n] returns true if [n] is empty. *)
val is_empty : 'a t -> bool

(** [bind n k v] returns a namespace with the value [v] bound to the key
    [k]. *)
val bind : 'a t -> string -> 'a -> 'a t

(** Alias for {!bind}. *)
val add : 'a t -> string -> 'a -> 'a t

(** [bind_all n l] returns a namespace with a binding for each of the
    key,value pairs in [l] *)
val bind_all : 'a t -> (string * 'a) list -> 'a t

(** [bind_value_list n k vs] returns a namespace with the values in [vs]
    bound to the key [k]. *)
val bind_value_list : 'a t -> string -> 'a list -> 'a t

(** [remove n k] removes the first value bound to [k] in [n]. Does nothing
    if k is not bound to anything. *)
val remove : 'a t -> string -> 'a t

(** [is_bound n k] returns true if [k] is bound in [n]. *)
val is_bound : 'a t -> string -> bool

(** [find n k] returns the first value associated with the key [k].
    @raise Not_found if there is no binding. *)
val find : 'a t -> string -> 'a

(** Same as {!find} except no exception is raised if there is no binding. *)
val findo : 'a t -> string -> 'a option

(** [find_all n k] returns all of the values associated with the key [k].
    @raise Not_found if there is no binding. *)
val find_all : 'a t -> string -> 'a list

(** Same as {!find_all} except no exception is raised if there is no
    binding. *)
val find_allo : 'a t -> string -> 'a list option

(** [extend m n] extends namespace [m] with all of the [(key,value)] pairs in
    [n]. *)
val extend : 'a t -> 'a t -> 'a t

(** [size n] returns the number of keys that are bound to values in [n]. *)
val size : 'a t -> int

(** [size_vals n] returns the number of values in [n]. *)
val size_vals : 'a t -> int

(** [iter f n] applies [f] to all bindings in [n]. [f] receives the key as
    first argument, then the associated value. *)
val iter : (string -> 'a -> unit) -> 'a t -> unit

(** [map f n] returns a namespace with the same domain as [n], where the
    all values have been replaced by the result of applying [f] to each
    value. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [mapi f n] is the same as {!map} except [f] receives both the key and
    associated value as arguments. *)
val mapi : (string -> 'a -> 'b) -> 'a t -> 'b t

(** [fold f n init] computes [(f kN vN ... (f k0 v0 init)...)], where [k0 ...
    kN] are the keys of all bindings in [n], and [v0 ... vN] are the
    associated values. *)
val fold : (string -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

(** [compare cmp m n] returns an int that describes how [m] and [n] compare.
    [cmp] is a total ordering used to compare values associated with equal
    keys in the two namespaces. *)
val compare : ('a list -> 'a list -> int) -> 'a t -> 'a t -> int

(** [equal cmp m n] tests whether the namespaces [m] and [n] are equal, that
    is, contain equal keys and associate them with equal values. [cmp] is the
    equality predicate used to compare the values associated with the keys. *)
val equal : ('a list -> 'a list -> bool) -> 'a t -> 'a t -> bool
