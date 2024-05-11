type walk =
  | W
  | NW
  | IW
  | INW

type terrain =
  | Grass
  | Path
  | Sand
  | Rock
  | Water

type tile = {
  interact : walk;
  terrain : terrain;
  deco1 : bool;
  deco2 : bool;
  texture : string;
}
(**[tile] is the type of tile of a top-down 2D roaming style map.

   "walk" Constructors: W - Walkable by player | NW - Not Walkable by player |
   IW - Interactable and Walkable by player (ex: stairs) | INW - Ineractable but
   not Walkable by player (ex: sign)*)
