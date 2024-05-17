open Meowsta
open MType
open Moves
open Items
open Abilities
open Status

(**[create_pokemon] takes in the parameters of the meowsta compilation unit and
   creates a corresponding meowsta*)
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

let pikachu =
  create_pokemon "Pikachu" (Electric, NO) NO 5 NO 100 14 10 8 NO
    [| shock; tackle; no; no |]
    [] 16 None

let meowleaf =
  create_pokemon "Meowleaf" (Grass, NO) NO 5 NO 100 14 10 8 NO
    [| absorb; tackle; no; no |]
    [] 16 None

let meowberger =
  create_pokemon "Meowberger" (Fire, NO) NO 5 NO 100 14 10 8 NO
    [| ember; tackle; no; no |]
    [] 16 None

let meowlder =
  create_pokemon "Meowlder" (Rock, NO) NO 5 NO 100 14 10 8 NO
    [| rock_toss; tackle; no; no |]
    [] 16 None

let catigue =
  create_pokemon "Catigue" (Psychic, NO) NO 5 NO 100 14 10 8 NO
    [| psychic; tackle; no; no |]
    [] 16 None

let purrfire =
  create_pokemon "Purrfire" (Fire, NO) NO 5 NO 100 14 10 8 NO
    [| ember; tackle; no; no |]
    [] 16 None

let purrtoad =
  create_pokemon "Purrtoad" (Water, Poison) NO 5 NO 120 18 14 12 NO
    [| water_gun; tackle; ice_ball; poison_jab |]
    [] 16 None

let clawson =
  create_pokemon "Clawson" (Ground, Dragon) NO 5 NO 100 14 10 8 NO
    [| take_down; tackle; no; no |]
    [] 16 None

let weak_meowberger = { meowberger with hp = 0 }

let meowtter =
  create_pokemon "Meowtter" (Water, NO) NO 5 NO 90 10 13 9 NO
    [| water_gun; tackle; no; no |]
    [] 16 None

let meowsta_list =
  [ pikachu; meowleaf; meowberger; clawson; meowtter; purrtoad ]
