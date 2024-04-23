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

let attack attacker defender move =
  let base_damage =
    float_of_int attacker.attack
    /. float_of_int defender.defense
    *. float_of_int move.damage
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

(** The evolve mechanic *)
let evolve m =
  m.level <- m.level + 1;
  if int_of_float (1.2 *. float_of_int m.hp) > 100 then m.hp <- 100
  else m.hp <- int_of_float (1.2 *. float_of_int m.hp);
  m.attack <- int_of_float (1.2 *. float_of_int m.attack);
  m.defense <- int_of_float (1.2 *. float_of_int m.defense);
  m.speed <- int_of_float (1.2 *. float_of_int m.speed);
  print_string (m.name ^ " has evolved!");

  (match m.item with
  | NO -> m.item <- LifeOrb
  | LifeOrb | MegaStone -> m.item <- MegaStone);

  match m.status with
  | NO -> m.status <- Burn
  | Burn -> m.status <- Poison
  | Poison -> m.status <- Sleep
  | Sleep | Paralysis -> m.status <- Paralysis

(** The catching mechanic *)
let try_catch m =
  let base_chance =
    match m.item with
    | LifeOrb -> 1.0
    | MegaStone -> 1.5
    | _ -> 0.5 (* Default value for non-catch items *)
  in
  let status_modifier =
    match m.status with
    | Sleep | Paralysis -> 2.0
    | Poison | Burn -> 1.5
    | NO -> 1.0
  in
  let hp_factor = float_of_int m.hp /. 100.0 in
  let chance = base_chance *. status_modifier /. hp_factor in
  Random.float 1.0 < chance
