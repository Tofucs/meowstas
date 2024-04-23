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
  mutable hp : int;
  mutable attack : int;
  mutable defense : int;
  mutable speed : int;
  moveset : moves array;
  mutable status : status;
}

val attack : t -> t -> moves -> unit
val is_dead : t -> bool
val evolve : t -> unit
val try_catch : t -> bool
