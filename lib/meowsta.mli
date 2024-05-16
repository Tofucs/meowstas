open MType
open Abilities
open Moves
open Items
open Status

type t = {
  name : string;
  poke_type : mtype * mtype;
  ability : abilities;
  evolution : int * t option;
  mutable exp : int;
  mutable level_threshold : int;
  mutable level : int;
  mutable item : items;
  mutable max_hp : int;
  mutable hp : int;
  mutable max_attack : int;
  mutable attack : int;
  mutable max_defense : int;
  mutable defense : int;
  mutable max_speed : int;
  mutable speed : int;
  mutable status : status;
  moveset : moves array;
  mutable learnset : (int * moves) list;
}

val get_item : t -> items
(** [get_item t] gets the t.item *)
val empty : t
(** [empty] is an empty type t*)
val attack : t -> t -> moves -> unit

val is_dead : t -> bool
(**[is_dead t] checks if [t.hp] is less than or equal to 0 and returns [true] if
   so. Returns [false] if not.*)

val check_levelup : t -> unit
val check_evolve : t -> t
val apply_booster_item : items -> moves -> float
