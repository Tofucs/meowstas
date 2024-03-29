type mtype =
  | NO
  | Normal
  | Fire
  | Water
  | Grass
  | Ice
  | Steel
  | Dragon
  | Fairy
  | Rock
  | Ground
  | Flying
  | Poison
  | Bug
  | Dark
  | Psychic
  | Fighting
  | Electric
  | Ghost

(**Note: call twice since pokemon have two types max*)
let type_multiplier (t1 : mtype) (t2 : mtype) =
  match (t1, t2) with
  | _, NO -> 1.
  | Normal, Ghost -> 0.
  | Normal, Rock -> 0.5
  | Normal, Steel -> 0.5
  | Fire, Fire -> 0.5
  | Fire, Water -> 0.5
  | Fire, Grass -> 2.
  | Fire, Ice -> 2.
  | Fire, Bug -> 2.
  | Fire, Rock -> 0.5
  | Fire, Steel -> 2.
  | Fire, Dragon -> 0.5
  | Water, Fire -> 2.
  | Water, Water -> 0.5
  | Water, Grass -> 0.5
  | Water, Ground -> 2.
  | Water, Rock -> 2.
  | Water, Dragon -> 0.5
  | _, _ -> 1.
