open Meowstas.Battle
open Meowstas.Meowsta_dictionary
open Meowstas.Poke_graphics

let () =
  init ();
  draw_interface ();
  close ();
  Printf.printf "A wild Meowtter appears!\n";
  battle_loop meowberger meowtter
