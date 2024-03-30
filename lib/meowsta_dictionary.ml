open Meowsta
open MType
open Moves
open Items
open Abilities

let meowberger =
  {
    name = "Meowberger";
    poke_type = (Fire, NO);
    ability = NO;
    level = 5;
    item = NO;
    hp = 100;
    attack = 14;
    defense = 10;
    speed = 8;
    moveset = [| water_gun; tackle; no; no |];
  }

let clawson =
  {
    name = "Purrfesor Meowchael Clawson";
    poke_type = (Ground, Dragon);
    ability = NO;
    level = 5;
    item = NO;
    hp = 100;
    attack = 14;
    defense = 10;
    speed = 8;
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
    hp = 90;
    attack = 10;
    defense = 13;
    speed = 9;
    moveset = [| water_gun; tackle; no; no |];
  }
