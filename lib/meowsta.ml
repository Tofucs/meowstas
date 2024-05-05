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

let apply_booster_item (item : items) (move : moves) =
  match item with
  | NO -> float_of_int move.damage
  | HardStone when move.attack_type = Rock -> 0.2 *. float_of_int move.damage
  | BlackBelt when move.attack_type = Fighting ->
      0.2 *. float_of_int move.damage
  | BlackGlasses when move.attack_type = Dark -> 0.2 *. float_of_int move.damage
  | Charcoal when move.attack_type = Fire -> 0.2 *. float_of_int move.damage
  | DragonFang when move.attack_type = Dragon -> 0.2 *. float_of_int move.damage
  | FairyFeather when move.attack_type = Fairy ->
      0.2 *. float_of_int move.damage
  | Magnet when move.attack_type = Electric -> 0.2 *. float_of_int move.damage
  | MetalCoat when move.attack_type = Steel -> 0.2 *. float_of_int move.damage
  | MiracleSeed when move.attack_type = Grass -> 0.2 *. float_of_int move.damage
  | MysticWater when move.attack_type = Water -> 0.2 *. float_of_int move.damage
  | NeverMeltIce when move.attack_type = Ice -> 0.2 *. float_of_int move.damage
  | PoisonBarb when move.attack_type = Poison -> 0.2 *. float_of_int move.damage
  | SharpBeak when move.attack_type = Flying -> 0.2 *. float_of_int move.damage
  | SilkScarf when move.attack_type = Normal -> 0.2 *. float_of_int move.damage
  | SilverPowder when move.attack_type = Bug -> 0.2 *. float_of_int move.damage
  | SoftSand when move.attack_type = Ground -> 0.2 *. float_of_int move.damage
  | SpellTag when move.attack_type = Ghost -> 0.2 *. float_of_int move.damage
  | TwistedSpoon when move.attack_type = Psychic ->
      0.2 *. float_of_int move.damage
  | _ -> float_of_int move.damage

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
