open Items
open Meowsta

val party_meowsta : t array
(**[party_meowsta] is the players current party of pokemon*)

val extra_meowsta : t list ref
(**[extra_mewosta] is the player's PC of pokemon. In other words, all pokemon
   that the trainer owns but is not in [party_meowsta]*)

val item_bag : items list ref
(**[item_bag] is the player's bag of items*)

val apply_medicine_item : items -> t -> unit
(**[apply_medicine_item item pokemon] uses [item] on [pokemon]. Requires: [item]
   is in [item_bag] and is a medicine item. [pokemon] must be the player's
   pokemon*)

val try_catch : items -> t -> bool
(**[try_catch item pokemon] uses [item] to try to catch [pokemon]. Returns
   [true] if caught and [false] if not. Requires: [item] is in [item_bag].
   [pokemon] must be in battle. [item] must be a catching item.*)

val add_item : items -> unit
(**[add_item item] simply adds [item] to [item_bag]*)

val add_meowsta : t -> unit
(**[add_mewosta pokemon] adds pokemon to the earliest open index in
   [party_meowsta]. If [party_meowsta] is full, it adds to [extra_meowsta]*)

val switch_meowsta : int -> t -> unit
(**[switch_mewosta index pokemon] takes [pokemon] from [extra_meowsta] and
   switches that [pokemon] with the pokemon at [party_mewosta.(index)].
   Requires: [pokemon] is in [extra_mewosta] before switch*)

val hold_item : items -> int -> unit
(**[hold_items item index] gives the [item] to the pokemon at the given [index].
   Requires: [item] is in [item_bag] and [party_mewosta.(index)] is not [empty]*)
