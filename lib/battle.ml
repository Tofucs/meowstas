open Meowsta
open Moves
open MType
open Status

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
  | NO -> enemy_pokemon.status <- enemy_pokemon.status
  | Recoil ->
      player_pokemon.hp <-
        player_pokemon.hp - int_of_float (0.25 *. float_of_int damage);
      Printf.printf "%s took recoil damage!\n" player_pokemon.name
  | Heal ->
      player_pokemon.hp <-
        player_pokemon.hp + int_of_float (0.2 *. float_of_int damage);
      Printf.printf "%s received some healing!\n" player_pokemon.name
  | x ->
      if Random.int 8 = 0 && enemy_pokemon.status = NO then begin
        enemy_pokemon.status <- x;
        Printf.printf "%s was %s!\n" enemy_pokemon.name (Status.string_wrap x);
        if x = Paralysis then
          enemy_pokemon.speed <-
            int_of_float (0.5 *. float_of_int enemy_pokemon.speed)
        else if x = Burn then
          enemy_pokemon.attack <-
            int_of_float (0.5 *. float_of_int enemy_pokemon.attack)
        else ()
      end
      else enemy_pokemon.status <- enemy_pokemon.status

let cap_hp (pokemon : t) =
  if is_dead pokemon then pokemon.hp <- 0
  else if pokemon.hp > pokemon.max_hp then pokemon.hp <- pokemon.max_hp
  else pokemon.hp <- pokemon.hp

let reset_stats (pokemon : t) =
  pokemon.speed <- pokemon.max_speed;
  pokemon.attack <- pokemon.max_attack;
  pokemon.defense <- pokemon.max_defense

let check_death player_pokemon enemy_pokemon =
  if is_dead player_pokemon && is_dead enemy_pokemon then
    Printf.printf "%s and %s both fainted!\n" player_pokemon.name
      enemy_pokemon.name
  else if is_dead player_pokemon then
    Printf.printf "%s fainted! You lose.\n" player_pokemon.name
  else if is_dead enemy_pokemon then begin
    Printf.printf "%s fainted! You win.\n" enemy_pokemon.name;
    player_pokemon.exp <- player_pokemon.exp + (enemy_pokemon.level * 50);
    check_levelup player_pokemon
  end
  else ()

let apply_status pokemon =
  match pokemon.status with
  | Poison ->
      pokemon.hp <-
        pokemon.hp - int_of_float (0.0625 *. float_of_int pokemon.max_hp);
      Printf.printf "%s took damage from poison.\n" pokemon.name
  | Burn ->
      pokemon.hp <-
        pokemon.hp - int_of_float (0.0625 *. float_of_int pokemon.max_hp);
      Printf.printf "%s took damage from burn.\n" pokemon.name
  | _ -> ()

let player_sleep_counter = ref 3
let enemy_sleep_counter = ref 3

let battle_turn player_pokemon enemy_pokemon chosen_move =
  if
    player_pokemon.status <> Paralysis
    || (player_pokemon.status = Paralysis && Random.int 4 <> 0)
    || player_pokemon.status <> Sleep
    || player_pokemon.status <> Frozen
    || (player_pokemon.status = Sleep || player_pokemon.status = Frozen)
       && !player_sleep_counter <= 0
  then begin
    if player_pokemon.status = Sleep || player_pokemon.status = Frozen then begin
      player_pokemon.status <- NO;
      player_sleep_counter := 3
    end
    else ();
    let old_enemy_hp = enemy_pokemon.hp in
    let () = attack player_pokemon enemy_pokemon chosen_move in
    cap_hp enemy_pokemon;
    Printf.printf "%s used %s! %s's HP is now %d\n" player_pokemon.name
      chosen_move.name enemy_pokemon.name enemy_pokemon.hp;
    check_effective chosen_move enemy_pokemon;

    if is_dead enemy_pokemon || is_dead player_pokemon then
      check_death player_pokemon enemy_pokemon
    else begin
      let damage = old_enemy_hp - enemy_pokemon.hp in
      inflict_status chosen_move damage player_pokemon enemy_pokemon;
      cap_hp player_pokemon;
      if is_dead enemy_pokemon || is_dead player_pokemon then
        check_death player_pokemon enemy_pokemon
    end
  end
  else if player_pokemon.status = Paralysis then
    print_endline "Couldn't move from Paralysis!"
  else if player_pokemon.status = Sleep then begin
    print_endline "Fast Asleep!";
    player_sleep_counter := !player_sleep_counter - 1
  end
  else begin
    print_endline "Frozen Solid!";
    player_sleep_counter := !player_sleep_counter - 1
  end

let battle_turn_enemy player_pokemon enemy_pokemon =
  if
    enemy_pokemon.status <> Paralysis
    || (enemy_pokemon.status = Paralysis && Random.int 4 <> 0)
    || enemy_pokemon.status <> Sleep
    || enemy_pokemon.status <> Frozen
    || (enemy_pokemon.status = Sleep || enemy_pokemon.status = Frozen)
       && !enemy_sleep_counter <= 0
  then begin
    if enemy_pokemon.status = Sleep || enemy_pokemon.status = Frozen then begin
      enemy_pokemon.status <- NO;
      enemy_sleep_counter := 3
    end
    else ();
    let enemy_move = enemy_pokemon.moveset.(0) in
    (* TODO: random *)
    let old_player_hp = player_pokemon.hp in
    let () = attack enemy_pokemon player_pokemon enemy_move in
    cap_hp player_pokemon;
    Printf.printf "%s used %s! %s's HP is now %d\n" enemy_pokemon.name
      enemy_move.name player_pokemon.name player_pokemon.hp;
    check_effective enemy_move player_pokemon;
    if is_dead enemy_pokemon || is_dead player_pokemon then
      check_death player_pokemon enemy_pokemon
    else begin
      let damage = old_player_hp - player_pokemon.hp in
      inflict_status enemy_move damage enemy_pokemon player_pokemon;
      cap_hp enemy_pokemon;
      if is_dead enemy_pokemon || is_dead player_pokemon then
        check_death player_pokemon enemy_pokemon
    end
  end
  else if enemy_pokemon.status = Paralysis then
    print_endline "Couldn't move from Paralysis!"
  else if enemy_pokemon.status = Sleep then begin
    print_endline "Fast Asleep!";
    enemy_sleep_counter := !enemy_sleep_counter - 1
  end
  else begin
    print_endline "Frozen Solid!";
    enemy_sleep_counter := !enemy_sleep_counter - 1
  end

let rec battle_loop player_pokemon enemy_pokemon =
  Array.iteri
    (fun i move -> Printf.printf "%d: %s\n" (i + 1) move.name)
    player_pokemon.moveset;
  Printf.printf "Choose a move (type number): ";
  let choice = read_int () in
  let chosen_move = player_pokemon.moveset.(choice - 1) in

  if player_pokemon.speed >= enemy_pokemon.speed then begin
    battle_turn player_pokemon enemy_pokemon chosen_move;
    if is_dead enemy_pokemon || is_dead player_pokemon then ()
    else begin
      battle_turn_enemy player_pokemon enemy_pokemon;
      if is_dead enemy_pokemon || is_dead player_pokemon then ()
      else begin
        apply_status enemy_pokemon;
        apply_status player_pokemon;
        battle_loop player_pokemon enemy_pokemon
      end
    end
  end
  else begin
    battle_turn_enemy player_pokemon enemy_pokemon;
    if is_dead enemy_pokemon || is_dead player_pokemon then ()
    else begin
      battle_turn player_pokemon enemy_pokemon chosen_move;
      if is_dead enemy_pokemon || is_dead player_pokemon then ()
      else begin
        apply_status enemy_pokemon;
        apply_status player_pokemon;
        battle_loop player_pokemon enemy_pokemon
      end
    end
  end
