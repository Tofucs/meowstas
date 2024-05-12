open Tile
open Player

type map
(**[map] is a top-down 2d-coordinate grid-like area that is walkable by the
   player, with ['t] as the type of tile in the grid. Keeps track of player
   location, size of the area being traversed, and the name of the area*)

type move_map_callback = string -> int * int -> unit

exception PlayerUninstantiated

val make : unit -> string -> tile array array -> int * int -> string list -> map
(**[make name grid (w, h)] makes a map area with the name [name], and its
   walkable and non walkable areas determined by the 2d array [grid], with width
   w and height h Requires: h is the length of the tile array array, and w is
   the length of each array in that array. This implicitly also means tile array
   array is rectangular *)

val create_player : map -> int option * int option -> unit
(**[create_player map (x,y)] creates a player with an initial position of (x, y)
   Requires: (x,y) is (Some int, Some int), otherwise it is not a valid position
   to create the player at.*)

val get_grid : map -> tile array array

val size : map -> int * int
(**[size map] returns the (width, height) of the [map]*)

val name : map -> string
(**[name map] returns the name the area described by [map]*)

val query_tile : map -> int option * int option -> tile
(**[query_tile x y] returns the tile at coordinates (x,y) on the map*)

val get_player_pos : map -> int option * int option
(**[get_player_pos map] gets the position of the player on the map*)

val get_player : map -> player

val update_location : map -> moves -> move_map_callback -> unit
(**[update_location map attempted_move] will attempt to move the player
   character in the direction of [attempted_move]. moves is a type with
   constructors Up, Down, Left, Right, each constructor corresponds to a
   movement on the x-y plane representation of the map by one unit. If the tile
   attempted to move to is not a walkable tile, it does not change the player
   position. Throws: PlayerUninstantiated if the player position isn't fully
   defined(not a Some int, Some int)*)
