open MType
open Abilities
open Moves
open Items
open Status

type t = {
  name : string;
  poke_type : mtype * mtype;
  ability : abilities;
  mutable level : int;
  mutable item : items;
  mutable max_hp : int;
  mutable hp : int;
  mutable attack : int;
  mutable defense : int;
  mutable speed : int;
  mutable status : status;
  moveset : moves array;
  mutable status : status;
}

val attack : t -> t -> moves -> unit

val is_dead : t -> bool
(**[is_dead t] checks if [t.hp] is less than or equal to 0 and returns [true] if
   so. Returns [false] if not.*)

val evolve : t -> unit
val try_catch : t -> bool

