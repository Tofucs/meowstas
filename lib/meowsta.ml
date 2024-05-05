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
  mutable max_attack : int;
  mutable attack : int;
  mutable max_defense : int;
  mutable defense : int;
  mutable max_speed : int;
  mutable speed : int;
  mutable status : status;
  moveset : moves array;
}

let attack attacker defender move =
  let base_damage =
    float_of_int attacker.attack
    /. float_of_int defender.defense
    *. apply_booster_item attacker.item move
  in
  defender.hp <-
    float_of_int defender.hp
    -. base_damage
       *. type_multiplier move.attack_type (fst defender.poke_type)
       *. type_multiplier move.attack_type (snd defender.poke_type)
    |> int_of_float

let is_dead pokemon =
  if pokemon.hp <= 0 then begin
    pokemon.hp <- 0;
    true
  end
  else false

(** TODO: Need to CHANGE *)
let evolve m =
  m.level <- m.level + 1;
  if int_of_float (1.2 *. float_of_int m.hp) > 100 then m.hp <- 100
  else m.hp <- int_of_float (1.2 *. float_of_int m.hp);
  m.attack <- int_of_float (1.2 *. float_of_int m.attack);
  m.defense <- int_of_float (1.2 *. float_of_int m.defense);
  m.speed <- int_of_float (1.2 *. float_of_int m.speed);
  print_string (m.name ^ " has evolved!")

(** TODO: NEED TO CHANGE*)
let try_catch m =
  let base_chance = 1.0 in

  let status_modifier =
    match m.status with
    | Sleep | Paralysis -> 2.0
    | Poison | Burn | Frozen -> 1.5
    | _ -> 1.0
  in
  let hp_factor = float_of_int m.hp /. 100.0 in
  let chance = base_chance *. status_modifier /. hp_factor in
  Random.float 1.0 < chance
