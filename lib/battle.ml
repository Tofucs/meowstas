
type move = {
  name_m: string;
  damage: int;
}

type pokemon = {name: string; hp : int; moves : move list}


 let is_dead pokemon = if pokemon.hp <= 0 then true else false

 let apply_move defender move = { defender with hp = defender.hp - move.damage }

 let rec battle_loop player_pokemon enemy_pokemon =
  if is_dead player_pokemon then
    Printf.printf "%s fainted! You lose.\n" player_pokemon.name
  else if is_dead enemy_pokemon then
    Printf.printf "%s fainted! You win.\n" enemy_pokemon.name
  else begin
    List.iteri (fun i move ->
        Printf.printf "%d: %s\n" (i + 1) move.name_m
      ) player_pokemon.moves;

    Printf.printf "Choose a move (type number): ";
    let choice = read_int () in
    let chosen_move = List.nth player_pokemon.moves (choice - 1) in

    let updated_enemy_pokemon = apply_move enemy_pokemon chosen_move in
    Printf.printf "%s used %s! %s's HP is now %d\n"
      player_pokemon.name chosen_move.name_m updated_enemy_pokemon.name updated_enemy_pokemon.hp;

    let enemy_move = List.hd enemy_pokemon.moves in
    let updated_player_pokemon = apply_move player_pokemon enemy_move in
    Printf.printf "%s used %s! %s's HP is now %d\n\n"
      enemy_pokemon.name enemy_move.name_m updated_player_pokemon.name updated_player_pokemon.hp;

    battle_loop updated_player_pokemon updated_enemy_pokemon
  end