open Meowstas.Battle

let pikachu_moves = [
  {name_m="Thunder Shock"; damage=40;};
  {name_m="Quick Attack"; damage=20;};
]

let bulbasaur_moves = [
  {name_m="Vine Whip"; damage=45};
  {name_m="Tackle"; damage=20};
]

let pikachu = {name="Pikachu"; hp=100; moves=pikachu_moves}
let bulbasaur = {name="Bulbasaur"; hp=100; moves=bulbasaur_moves}

let () =
  Printf.printf "A wild Bulbasaur appears!\n";
  battle_loop pikachu bulbasaur