open Meowsta
open Moves
open MType

let check_effective (move : moves) (enemy_pokemon : t) =
  let multiplier =
    type_multiplier move.attack_type (fst enemy_pokemon.poke_type)
    *. type_multiplier move.attack_type (snd enemy_pokemon.poke_type)
  in
  if multiplier >= 2. then print_endline "It's super effective!"
  else if multiplier < 1. then print_endline "It's not very effective"
  else print_endline ""

let rec battle_loop player_pokemon enemy_pokemon =
  if is_dead player_pokemon then
    Printf.printf "%s fainted! You lose.\n" player_pokemon.name
  else if is_dead enemy_pokemon then
    Printf.printf "%s fainted! You win.\n" enemy_pokemon.name
  else begin
    Array.iteri
      (fun i move -> Printf.printf "%d: %s\n" (i + 1) move.name)
      player_pokemon.moveset;

    Printf.printf "Choose a move (type number): ";
    let choice = read_int () in
    let chosen_move = player_pokemon.moveset.(choice - 1) in

    let () = attack player_pokemon enemy_pokemon chosen_move in
    Printf.printf "%s used %s! %s's HP is now %d\n" player_pokemon.name
      chosen_move.name enemy_pokemon.name enemy_pokemon.hp;
    check_effective chosen_move enemy_pokemon;

    let enemy_move = enemy_pokemon.moveset.(0) in
    let () = attack enemy_pokemon player_pokemon enemy_move in
    Printf.printf "%s used %s! %s's HP is now %d\n\n" enemy_pokemon.name
      enemy_move.name player_pokemon.name player_pokemon.hp;
    check_effective enemy_move player_pokemon;

    battle_loop player_pokemon enemy_pokemon
  end
