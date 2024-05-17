open Meowsta
open Moves
open MType
open Status

let () = Random.self_init ()
let text_display = ref None
let set_text_display (display : Label.t) = text_display := Some display

let check_effective (move : moves) (enemy_pokemon : t) =
  let multiplier =
    type_multiplier move.attack_type (fst enemy_pokemon.poke_type)
    *. type_multiplier move.attack_type (snd enemy_pokemon.poke_type)
  in
  if multiplier >= 2. then
    Label.add_text (Option.get !text_display) "\nIt's super effective!"
  else if multiplier < 1. then
    Label.add_text (Option.get !text_display) "\nIt's not very effective"
  else print_endline ""

let inflict_status (move : moves) (damage : int) (player_pokemon : t)
    (enemy_pokemon : t) =
  match move.status_effect with
  | NO -> enemy_pokemon.status <- enemy_pokemon.status
  | Recoil ->
      player_pokemon.hp <-
        player_pokemon.hp - int_of_float (0.25 *. float_of_int damage);
      Printf.sprintf "\n%s took recoil damage!" player_pokemon.name
      |> Label.add_text (Option.get !text_display)
  | Heal ->
      player_pokemon.hp <-
        player_pokemon.hp + int_of_float (0.2 *. float_of_int damage);
      Printf.sprintf "\n%s received some healing!" player_pokemon.name
      |> Label.add_text (Option.get !text_display)
  | x ->
      if Random.int 8 = 0 && enemy_pokemon.status = NO then begin
        enemy_pokemon.status <- x;
        Printf.sprintf "\n%s was %s!" enemy_pokemon.name (Status.string_wrap x)
        |> Label.add_text (Option.get !text_display);
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
  pokemon.defense <- pokemon.max_defense;
  pokemon.hp <- pokemon.max_hp

let check_death player_pokemon enemy_pokemon =
  if is_dead player_pokemon && is_dead enemy_pokemon then
    Printf.sprintf "\n%s and %s both fainted!" player_pokemon.name
      enemy_pokemon.name
    |> Label.add_text (Option.get !text_display)
  else if is_dead player_pokemon then
    Printf.sprintf "\n%s fainted! You lose." player_pokemon.name
    |> Label.add_text (Option.get !text_display)
  else if is_dead enemy_pokemon then begin
    Printf.sprintf "\n%s fainted! You win." enemy_pokemon.name
    |> Label.add_text (Option.get !text_display);
    player_pokemon.exp <- player_pokemon.exp + (enemy_pokemon.level * 50);
    check_levelup player_pokemon
  end
  else ()

let apply_status pokemon =
  match pokemon.status with
  | Poison ->
      pokemon.hp <-
        pokemon.hp - int_of_float (0.0625 *. float_of_int pokemon.max_hp);
      Printf.sprintf "\n%s took damage from poison." pokemon.name
      |> Label.add_text (Option.get !text_display)
  | Burn ->
      pokemon.hp <-
        pokemon.hp - int_of_float (0.0625 *. float_of_int pokemon.max_hp);
      Printf.sprintf "\n%s took damage from burn." pokemon.name
      |> Label.add_text (Option.get !text_display)
  | _ -> ()

(*TODO: IMPLEMENT LEVEL UP + ADDING MOVES. IMPLEMENT ABILITIES. Implement
  Evolution *)

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
    Printf.sprintf "\n%s used %s! %s's HP is now %d" player_pokemon.name
      chosen_move.name enemy_pokemon.name enemy_pokemon.hp
    |> Label.add_text (Option.get !text_display);
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
    "\nCouldn't move from Paralysis!"
    |> Label.add_text (Option.get !text_display)
  else if player_pokemon.status = Sleep then begin
    "\nFast Asleep!" |> Label.add_text (Option.get !text_display);
    player_sleep_counter := !player_sleep_counter - 1
  end
  else begin
    "\nFrozen Solid!" |> Label.add_text (Option.get !text_display);
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
    Printf.sprintf "\n%s used %s! %s's HP is now %d" enemy_pokemon.name
      enemy_move.name player_pokemon.name player_pokemon.hp
    |> Label.add_text (Option.get !text_display);
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
    "\nCouldn't move from Paralysis!"
    |> Label.add_text (Option.get !text_display)
  else if enemy_pokemon.status = Sleep then begin
    "\nFast Asleep!" |> Label.add_text (Option.get !text_display);
    enemy_sleep_counter := !enemy_sleep_counter - 1
  end
  else begin
    "\nFrozen Solid!" |> Label.add_text (Option.get !text_display);
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
