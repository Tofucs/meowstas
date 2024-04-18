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

(*TODO: IMPLEMENT SLEEP/FROZEN STATUS. IMPLEMENT ITEMS. DO SLEEP STUFF FOR
  ENEMY*)

let player_sleep_counter = ref 3
let enemy_sleep_counter = ref 3

let battle_turn player_pokemon enemy_pokemon chosen_move =
  if
    player_pokemon.status <> Paralysis
    || (player_pokemon.status = Paralysis && Random.int 4 <> 0)
    || player_pokemon.status <> Sleep
    || (player_pokemon.status = Sleep && !player_sleep_counter <= 0)
  then begin
    if player_pokemon.status = Sleep then begin
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
  else print_endline "Fast Asleep!";
  player_sleep_counter := !player_sleep_counter - 1

let battle_turn_enemy player_pokemon enemy_pokemon =
  if
    enemy_pokemon.status <> Paralysis
    || (enemy_pokemon.status = Paralysis && Random.int 4 <> 0)
    || enemy_pokemon.status <> Sleep
    || (enemy_pokemon.status = Sleep && !enemy_sleep_counter <= 0)
  then begin
    if enemy_pokemon.status = Sleep then begin
      enemy_pokemon.status <- NO;
      enemy_sleep_counter := 3
    end
    else ();
    let enemy_move = enemy_pokemon.moveset.(0) in
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
  else print_endline "Fast Asleep!";
  enemy_sleep_counter := !enemy_sleep_counter - 1

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
      else battle_loop player_pokemon enemy_pokemon
    end
  end
  else begin
    battle_turn_enemy player_pokemon enemy_pokemon;
    if is_dead enemy_pokemon || is_dead player_pokemon then ()
    else begin
      battle_turn player_pokemon enemy_pokemon chosen_move;
      if is_dead enemy_pokemon || is_dead player_pokemon then ()
      else battle_loop player_pokemon enemy_pokemon
    end
  end

(*let rec battle_loop player_pokemon enemy_pokemon = begin Array.iteri (fun i
  move -> Printf.printf "%d: %s\n" (i + 1) move.name) player_pokemon.moveset;

  Printf.printf "Choose a move (type number): "; let choice = read_int () in let
  chosen_move = player_pokemon.moveset.(choice - 1) in

  if player_pokemon.status <> Paralysis || (player_pokemon.status = Paralysis &&
  Random.int 4 <> 0) then begin let old_enemy_hp = enemy_pokemon.hp in let () =
  attack player_pokemon enemy_pokemon chosen_move in cap_hp enemy_pokemon;
  Printf.printf "%s used %s! %s's HP is now %d\n" player_pokemon.name
  chosen_move.name enemy_pokemon.name enemy_pokemon.hp; check_effective
  chosen_move enemy_pokemon;

  if is_dead enemy_pokemon || is_dead player_pokemon then check_death
  player_pokemon enemy_pokemon else begin let damage = old_enemy_hp -
  enemy_pokemon.hp in inflict_status chosen_move damage player_pokemon
  enemy_pokemon; cap_hp player_pokemon; if is_dead enemy_pokemon || is_dead
  player_pokemon then check_death player_pokemon enemy_pokemon end end else
  print_endline "Couldn't move from Paralysis!"; if enemy_pokemon.status <>
  Paralysis || (enemy_pokemon.status = Paralysis && Random.int 4 <> 0) then
  begin let enemy_move = enemy_pokemon.moveset.(0) in let old_player_hp =
  player_pokemon.hp in let () = attack enemy_pokemon player_pokemon enemy_move
  in cap_hp player_pokemon; Printf.printf "%s used %s! %s's HP is now %d\n"
  enemy_pokemon.name enemy_move.name player_pokemon.name player_pokemon.hp;
  check_effective enemy_move player_pokemon; if is_dead enemy_pokemon || is_dead
  player_pokemon then check_death player_pokemon enemy_pokemon else begin let
  damage = old_player_hp - player_pokemon.hp in inflict_status chosen_move
  damage enemy_pokemon player_pokemon; cap_hp enemy_pokemon; if is_dead
  enemy_pokemon || is_dead player_pokemon then check_death player_pokemon
  enemy_pokemon else battle_loop player_pokemon enemy_pokemon end end else begin
  print_endline "Couldn't move from Paralysis!"; battle_loop player_pokemon
  enemy_pokemon end end*)
