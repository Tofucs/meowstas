open Meowsta
open MType
open Moves
open Items
open Abilities
open Status

(**Add Learnset*)
let create_pokemon name poke_type ability level item max_hp max_attack
    max_defense max_speed status moveset learnset evolve_lv evolve_meowsta =
  {
    name;
    poke_type;
    ability;
    level;
    evolution = (evolve_lv, evolve_meowsta);
    exp = 0;
    level_threshold = level * 200;
    item;
    max_hp;
    hp = max_hp;
    max_attack;
    attack = max_attack;
    max_defense;
    defense = max_defense;
    max_speed;
    speed = max_speed;
    status;
    moveset;
    learnset;
  }

let meowberger =
  create_pokemon "Meowberger" (Fire, NO) NO 5 NO 100 14 10 8 NO
    [| ember; tackle; no; no |]
    [] 16 None

let clawson =
  create_pokemon "Purrfesor Meowchael Clawson" (Ground, Dragon) NO 5 NO 100 14
    10 8 NO
    [| water_gun; tackle; no; no |]
    [] 16 None

let weak_meowberger = { meowberger with hp = 0 }

let meowtter =
  create_pokemon "Meowtter" (Water, NO) NO 5 NO 90 10 13 9 NO
    [| water_gun; tackle; no; no |]
    [] 16 None
