open Meowsta
open Moves
open MType

let () = Random.self_init ()

let check_effective (move : moves) (enemy_pokemon : t) =
  let multiplier =
    type_multiplier move.attack_type (fst enemy_pokemon.poke_type)
    *. type_multiplier move.attack_type (snd enemy_pokemon.poke_type)
  in
  if multiplier >= 2. then print_endline "It's super effective!"
  else if multiplier < 1. then print_endline "It's not very effective"
  else print_endline ""

let inflict_status (move : moves) (damage : int) (player_pokemon : t)
    (enemy_pokemon : t) =
  match move.status_effect with
  | NO -> enemy_pokemon.status <- NO
  | Recoil ->
      player_pokemon.hp <-
        player_pokemon.hp - int_of_float (0.25 *. float_of_int damage);
      Printf.printf "%s took recoil damage!\n" player_pokemon.name
  (*NOTE: NEED TO IMPLEMENT HP CAP!!*)
  | Heal ->
      player_pokemon.hp <-
        player_pokemon.hp + int_of_float (0.2 *. float_of_int damage);
      Printf.printf "%s received some healing!\n" player_pokemon.name
  | x ->
      if Random.int 12 = 0 then begin
        enemy_pokemon.status <- x;
        Printf.printf "%s was %s!\n" enemy_pokemon.name (Status.string_wrap x)
      end
      else enemy_pokemon.status <- enemy_pokemon.status

let cap_hp (pokemon : t) =
  if is_dead pokemon then pokemon.hp <- 0
  else if pokemon.hp > pokemon.max_hp then pokemon.hp <- pokemon.max_hp
  else pokemon.hp <- pokemon.hp

let check_death player_pokemon enemy_pokemon =
  if is_dead player_pokemon && is_dead enemy_pokemon then
    Printf.printf "%s and %s both fainted!\n" player_pokemon.name
      enemy_pokemon.name
  else if is_dead player_pokemon then
    Printf.printf "%s fainted! You lose.\n" player_pokemon.name
  else if is_dead enemy_pokemon then
    Printf.printf "%s fainted! You win.\n" enemy_pokemon.name
  else ()

let apply_status pokemon =
  match pokemon.status with
  | Poison ->
      pokemon.hp <-
        pokemon.hp - int_of_float (0.0625 *. float_of_int pokemon.max_hp)
  (*NOTE: WE MUST FIND A WAY TO RESET ATTACK*)
  | Burn ->
      pokemon.hp <-
        pokemon.hp - int_of_float (0.0625 *. float_of_int pokemon.max_hp);
      pokemon.attack <- int_of_float (0.5 *. float_of_int pokemon.attack)
  (*NOTE: WE MUST FIND A WAY TO RESET SPEED*)
  | Paralysis ->
      pokemon.speed <- int_of_float (0.5 *. float_of_int pokemon.speed)
  | _ -> ()

let rec battle_loop player_pokemon enemy_pokemon =
  begin
    Array.iteri
      (fun i move -> Printf.printf "%d: %s\n" (i + 1) move.name)
      player_pokemon.moveset;

    Printf.printf "Choose a move (type number): ";
    let choice = read_int () in
    let chosen_move = player_pokemon.moveset.(choice - 1) in

    if
      player_pokemon.status <> Paralysis
      || (player_pokemon.status = Paralysis && Random.int 4 <> 0)
    then begin
      let old_enemy_hp = enemy_pokemon.hp in
      let () = attack player_pokemon enemy_pokemon chosen_move in
      cap_hp enemy_pokemon;
      Printf.printf "%s used %s! %s's HP is now %d\n" player_pokemon.name
        chosen_move.name enemy_pokemon.name enemy_pokemon.hp;
      check_effective chosen_move enemy_pokemon;

      if is_dead enemy_pokemon || is_dead player_pokemon then
        check_death player_pokemon enemy_pokemon
      else
        let damage = old_enemy_hp - enemy_pokemon.hp in
        inflict_status chosen_move damage player_pokemon enemy_pokemon;
        cap_hp player_pokemon
    end
    else print_endline "Couldn't move from Paralysis!";

    if is_dead enemy_pokemon || is_dead player_pokemon then
      check_death player_pokemon enemy_pokemon
    else if
      enemy_pokemon.status <> Paralysis
      || (enemy_pokemon.status = Paralysis && Random.int 4 <> 0)
    then begin
      let enemy_move = enemy_pokemon.moveset.(0) in
      let old_player_hp = player_pokemon.hp in
      let () = attack enemy_pokemon player_pokemon enemy_move in
      cap_hp player_pokemon;
      Printf.printf "%s used %s! %s's HP is now %d\n" enemy_pokemon.name
        enemy_move.name player_pokemon.name player_pokemon.hp;
      check_effective enemy_move player_pokemon;
      if is_dead enemy_pokemon || is_dead player_pokemon then
        check_death player_pokemon enemy_pokemon
      else
        let damage = old_player_hp - player_pokemon.hp in
        inflict_status chosen_move damage enemy_pokemon player_pokemon;
        cap_hp enemy_pokemon
    end
    else print_endline "Couldn't move from Paralysis!";
    if is_dead enemy_pokemon || is_dead player_pokemon then
      check_death player_pokemon enemy_pokemon
    else battle_loop player_pokemon enemy_pokemon
  end
