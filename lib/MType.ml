(**type [mytype] represents the catalog of all pokemon types*)
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

(**[type_multiplier t1 t2] checks the moves type with the enemy pokemons type to
   find the damage multiplier*)
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
  | Electric, Water -> 2.
  | Electric, Electric -> 0.5
  | Electric, Grass -> 0.5
  | Electric, Ground -> 0.
  | Electric, Flying -> 2.
  | Electric, Dragon -> 0.5
  | Grass, Fire -> 0.5
  | Grass, Water -> 2.
  | Grass, Grass -> 0.5
  | Grass, Poison -> 0.5
  | Grass, Ground -> 2.
  | Grass, Flying -> 0.5
  | Grass, Bug -> 0.5
  | Grass, Rock -> 2.
  | Grass, Dragon -> 0.5
  | Grass, Steel -> 0.5
  | Ice, Fire -> 0.5
  | Ice, Water -> 0.5
  | Ice, Grass -> 2.
  | Ice, Ice -> 0.5
  | Ice, Ground -> 2.
  | Ice, Flying -> 2.
  | Ice, Dragon -> 2.
  | Ice, Steel -> 0.5
  | Fighting, Normal -> 2.
  | Fighting, Ice -> 2.
  | Fighting, Poison -> 0.5
  | Fighting, Flying -> 0.5
  | Fighting, Psychic -> 0.5
  | Fighting, Bug -> 0.5
  | Fighting, Rock -> 2.
  | Fighting, Ghost -> 0.
  | Fighting, Dark -> 2.
  | Fighting, Steel -> 2.
  | Fighting, Fairy -> 0.5
  | Poison, Grass -> 2.
  | Poison, Poison -> 0.5
  | Poison, Ground -> 0.5
  | Poison, Rock -> 0.5
  | Poison, Ghost -> 0.5
  | Poison, Steel -> 0.
  | Poison, Fairy -> 2.
  | Ground, Fire -> 2.
  | Ground, Electric -> 2.
  | Ground, Grass -> 0.5
  | Ground, Poison -> 2.
  | Ground, Flying -> 0.
  | Ground, Bug -> 0.5
  | Ground, Rock -> 2.
  | Ground, Steel -> 2.
  | Flying, Electric -> 0.5
  | Flying, Grass -> 2.
  | Flying, Fighting -> 2.
  | Flying, Bug -> 2.
  | Flying, Rock -> 0.5
  | Flying, Steel -> 0.5
  | Psychic, Fighting -> 2.
  | Psychic, Poison -> 2.
  | Psychic, Psychic -> 0.5
  | Psychic, Dark -> 0.
  | Psychic, Steel -> 0.5
  | Bug, Fire -> 0.5
  | Bug, Grass -> 2.
  | Bug, Fighting -> 0.5
  | Bug, Poison -> 0.5
  | Bug, Flying -> 0.5
  | Bug, Psychic -> 2.
  | Bug, Ghost -> 0.5
  | Bug, Dark -> 2.
  | Bug, Steel -> 0.5
  | Bug, Fairy -> 0.5
  | Rock, Fire -> 2.
  | Rock, Ice -> 2.
  | Rock, Fighting -> 0.5
  | Rock, Ground -> 0.5
  | Rock, Flying -> 2.
  | Rock, Bug -> 2.
  | Rock, Steel -> 0.5
  | Ghost, Normal -> 0.
  | Ghost, Psychic -> 2.
  | Ghost, Ghost -> 2.
  | Ghost, Dark -> 0.5
  | Dragon, Dragon -> 2.
  | Dragon, Steel -> 0.5
  | Dragon, Fairy -> 0.
  | Dark, Fighting -> 0.5
  | Dark, Psychic -> 2.
  | Dark, Ghost -> 2.
  | Dark, Dark -> 0.5
  | Dark, Fairy -> 0.5
  | Steel, Fire -> 0.5
  | Steel, Water -> 0.5
  | Steel, Electric -> 0.5
  | Steel, Ice -> 2.
  | Steel, Rock -> 2.
  | Steel, Steel -> 0.5
  | Steel, Fairy -> 2.
  | Fairy, Fire -> 0.5
  | Fairy, Fighting -> 2.
  | Fairy, Poison -> 0.5
  | Fairy, Dragon -> 2.
  | Fairy, Dark -> 2.
  | Fairy, Steel -> 0.5
  | _, _ -> 1.
