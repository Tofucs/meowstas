open Meowsta
open MType
open Moves
open Items
open Abilities
open Status

let meowberger =
  {
    name = "Meowberger";
    poke_type = (Fire, NO);
    ability = NO;
    level = 5;
    item = NO;
    max_hp = 100;
    hp = 100;
    max_attack = 14;
    attack = 14;
    max_defense = 10;
    defense = 10;
    max_speed = 8;
    speed = 8;
    status = NO;
    moveset = [| water_gun; tackle; no; no |];
  }

let clawson =
  {
    name = "Purrfesor Meowchael Clawson";
    poke_type = (Ground, Dragon);
    ability = NO;
    level = 5;
    item = NO;
    max_hp = 100;
    hp = 100;
    max_attack = 14;
    attack = 14;
    max_defense = 10;
    defense = 10;
    max_speed = 8;
    speed = 8;
    status = NO;
    moveset = [| water_gun; tackle; no; no |];
  }

let weak_meowberger = { meowberger with hp = 0 }

let meowtter =
  {
    name = "Meowtter";
    poke_type = (Water, NO);
    ability = NO;
    level = 5;
    item = NO;
    max_hp = 90;
    hp = 90;
    max_attack = 10;
    attack = 10;
    max_defense = 13;
    defense = 13;
    max_speed = 9;
    speed = 9;
    status = NO;
    moveset = [| water_gun; tackle; no; no |];
  }
