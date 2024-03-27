open Meowsta
open MType
open Status 
open Moves 
open Items 
open Abilities


module Meowberger = struct
  let name = "Meowberger"
  let poke_type = (Fire, NO)
  let ability = NO
  let level = 5
  let item = NO

  let base_hp = 10
  let base_attack = 14
  let base_defence = 10
  let base_speed = 8
  let moveset = [|water_gun; tackle; no; no|]
end

module Meowtter = struct
  let name = "Meowtter"
  let poke_type = (Water, NO)
  let ability = NO
  let level = 5
  let item = NO

  let base_hp = 9
  let base_attack = 10
  let base_defence = 13
  let base_speed = 9
  let moveset = [|water_gun; tackle; no; no|]
end

