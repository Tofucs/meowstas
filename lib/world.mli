open Map

type game_map = map list
(** Type alias for a list of maps. *)

type world = {
  game_map : game_map;
  map_table : (string, map) Hashtbl.t;
  mutable current_location : string;
}
(** Abstract type representing the world containing game maps and other
    information. *)

val instance : world option ref
(** [instace] is the Reference to the optional world instance, initially None. *)

val initialize : unit -> unit
(** [intiialize] Initializes the game world with predefined maps and sets the
    initial location. *)

val get_instance : unit -> world
(** [get_instance] Returns the current instance of the world, raising an error
    if uninitialized. *)

val get_map : string -> map
(** [get_map] Retrieves a map by name from the world's map table, raising an
    error if the map is not found. *)

val update_map : string -> int * int -> unit
(** [update_map] Updates the current location in the world to the specified map
    and coordinates, raising an error if the map name is not found. *)
