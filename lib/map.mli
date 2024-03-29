open Tile
open Player

type map
(**[map] is a top-down 2d-coordinate grid-like map that is walkable by the
   player, with ['t] as the type of tile in the grid. Keeps track of player
   location*)

val size : map -> int * int
(**[size map] returns the (width, height) of the [map]*)

val query_tile : map -> int -> int -> tile
(**[query_tile x y] returns the tile at coordinates (x,y) on the map*)

val update_location : player -> moves -> int * int
