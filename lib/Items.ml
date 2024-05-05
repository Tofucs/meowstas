open Moves
open Type
open Meowsta
open Status

type items =
  | NO
  (*Booster*)
  | HardStone
  | BlackBelt
  | BlackGlasses
  | Charcoal
  | DragonFang
  | FairyFeather
  | Magnet
  | MetalCoat
  | MiracleSeed
  | MysticWater
  | NeverMeltIce
  | PoisonBarb
  | SharpBeak
  | SilkScarf
  | SilverPowder
  | SoftSand
  | SpellTag
  | TwistedSpoon
  (*Medicine*)
  | Potion
  | SuperPotion
  | HyperPotion
  | Revive
  | ParalyzeHeal
  | BurnHeal
  | Antidote
  | FreezeHeal
  | SleepHeal

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

let apply_medicine_item (item : items) (pokemon : t) =
  let match_medicine item pokemon =
    match item with
    | Potion -> pokemon.hp <- pokemon.hp + 20
    | SuperPotion -> pokemon.hp <- pokemon.hp + 50
    | HyperPotion -> pokemon.hp <- pokemon.hp + 100
    | Revive when pokemon.hp <= 0 -> pokemon.hp <- 1 / 3 * pokemon.max_hp
    | ParalyzeHeal when pokemon.status = Paralysis ->
        pokemon.status <- NO;
        pokemon.speed <- pokemon.max_speed
    | BurnHeal when pokemon.status = Burn ->
        pokemon.status <- NO;
        pokemon.attack <- pokemon.max_attack
    | Antidote when pokemon.status = Poison -> pokemon.status <- NO
    | FreezeHeal when pokemon.status = Frozen -> pokemon.status <- NO
    | SleepHeal when pokemon.status = Sleep -> pokemon.status <- NO
    | _ -> Printf.printf "Invalid Item!"
  in
  begin
    match_medicine item pokemon;
    if pokemon.hp > pokemon.max_hp then pokemon.hp <- pokemon.max_hp else ()
  end
