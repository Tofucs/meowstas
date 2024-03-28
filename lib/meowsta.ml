open MType
open Abilities
open Moves
open Items


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

  moveset : moves array
}

let attack attacker defender move = defender.hp <- (defender.defense / 100) * defender.hp - (attacker.attack / 100) * move.damage
