(**[tile] is the type of tile of a top-down 2D roaming style map. Constructors:
   W - Walkable by player | NW - Not Walkable by player | IW - Interactable and
   Walkable by player (ex: stairs) | INW - Ineractable but not Walkable by
   player (ex: sign)*)
type tile =
  | W
  | NW
  | IW
  | INW
