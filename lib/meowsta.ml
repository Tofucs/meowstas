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
  moveset : moves array;
}

let attack attacker defender move =
  defender.hp <-
    float_of_int defender.hp
    -. float_of_int attacker.attack
       /. float_of_int defender.defense
       *. float_of_int move.damage
    |> int_of_float

let is_dead pokemon = if pokemon.hp <= 0 then true else false
