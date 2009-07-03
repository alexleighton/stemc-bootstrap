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

module M = Map.Make(String)

(* A namespace record. Contains a list of values for each key. *)
type 'a t = { dict : 'a list M.t }

let create () = { dict = M.empty }

let is_empty n = M.is_empty n.dict

let bind n k v =
  { dict =
      if M.mem k n.dict then
        let vs = M.find k n.dict in M.add k (v::vs) n.dict
      else M.add k [v] n.dict
  }

let add = bind

let bind_all n l = List.fold_left (fun n (k,v) -> bind n k v) n l

let bind_value_list n k vs =
  { dict =
      if M.mem k n.dict then
        let vs' = M.find k n.dict in M.add k (vs @ vs') n.dict
      else M.add k vs n.dict
  }

let remove n k = let n = n.dict in
  { dict =
      if M.mem k n then
        match M.find k n with
          | [v]     -> M.remove k n
          | (v::vs) -> M.add k vs n
          | _       -> assert false
      else n
  }

let is_bound n k = M.mem k n.dict

let find n k = List.hd (M.find k n.dict)

let findo n k =
  try Some (List.hd (M.find k n.dict))
  with Not_found -> None

let find_all n k = M.find k n.dict

let find_allo n k =
  try Some (M.find k n.dict)
  with Not_found -> None

let extend a b = M.fold (fun k vs n -> bind_value_list n k vs) b.dict a

let size n = let i = ref 0 in
  M.iter (fun k vs -> incr i) n.dict;
  !i

let size_vals n = let i = ref 0 in
  M.iter (fun k vs -> i := List.length vs) n.dict;
  !i

let iter f n = M.iter (fun k vs -> f k (List.hd vs)) n.dict

let map f n = { dict = M.map (fun vs -> List.map f vs) n.dict }

let mapi f n = { dict = M.mapi (fun k vs -> List.map (f k) vs) n.dict }

let fold f n init = M.fold
  (fun k vs acc -> List.fold_right (fun v acc -> f k v acc) vs acc)
  n.dict init

let compare f m n = M.compare f m.dict n.dict

let equal f m n = M.equal f m.dict n.dict
