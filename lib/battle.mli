open Meowsta
open Moves
open MType
open Status

val check_effective : moves -> t -> unit
(**[check_effective move enemy_pokemon] checks if [move] created a super
   effective hit against [enemy_pokemon] and prints the information to the user *)

val cap_hp : t -> unit
(**[cap_hp pokemon] reverts the pokemon's hp to its max value in the case that
   it is exceeded*)

val reset_stats : t -> unit
(**[reset_stats pokemon] reverts all of the pokemon's stats to its original
   value*)

val check_death : t -> t -> unit
(**[check_death pokemon enemy_pokemon] checks if either the player or the enemy
   is dead or below 0 hp. Prints a message to the user if so.*)

val apply_status : t -> unit
(**[apply_status pokemon] applies a status effect on the affected [pokemon]*)

val battle_turn : t -> t -> moves -> unit
(**[battle_turn player enemy move] is the player's battle turn in which he
   attacks [enemy] given the chosen [move]*)

val battle_turn_enemy : t -> t -> unit
(**[battle_turn_enemy player enemy] is the enemy's battle turn in which he
   attacks [player] given a randomly chosen move*)

val battle_loop : t -> t -> unit
(**[battle_loop player enemy] represents the entire battle functino between
   [player] and [enemy]. There is move selection, status conditions, item
   effects, and more. Terminates once one pokemon dies*)
