open Tsdl
open Tile
open Tsdl_image
open Tsdl_ttf
open Main_utils
open Meowsta
open Meowsta_dictionary
open Battle

module BattleMode : GameMode = struct
  type t = global_state

  (* positions start from top left, rectangle is defined by top left as well *)
  let pos1 = (425, 575)
  let pos2 = (1100, 325)
  let in_animation = ref false
  let animation_start = ref 0
  let animation_duration = ref 0
  let start_pos = ref (0, 0)
  let end_pos = ref (0, 0)
  let background_file = "textures/battle-background.png"
  let player = ref pikachu
  let enemy = ref meowleaf
  let player_move = ref None

  let preload_textures renderer texture_table =
    let texture = load_texture_from_png renderer background_file in
    Hashtbl.add texture_table background_file texture;
    List.iter
      (fun meowsta ->
        let path = "textures/" ^ meowsta.name ^ ".png" in
        if not (Hashtbl.mem texture_table path) then
          let texture = load_texture_from_png renderer path in
          Hashtbl.add texture_table meowsta.name texture)
      Meowsta_dictionary.meowsta_list

  let run_turn () =
    if !player.speed >= !enemy.speed then begin
      battle_turn !player !enemy (Option.get !player_move);
      if is_dead !enemy || is_dead !player then ()
      else begin
        battle_turn_enemy !player !enemy;
        if is_dead !enemy || is_dead !player then ()
        else begin
          apply_status !enemy;
          apply_status !player
        end
      end
    end
    else begin
      battle_turn_enemy !player !enemy;
      if is_dead !enemy || is_dead !player then ()
      else begin
        battle_turn !player !enemy (Option.get !player_move);
        if is_dead !enemy || is_dead !player then ()
        else begin
          apply_status !enemy;
          apply_status !player
        end
      end
    end;
    if is_dead !enemy then 0 else if is_dead !player then 1 else 2

  let init state =
    if state.battle_state = None then (
      state.battle_state <-
        Some
          {
            player = Meowsta_dictionary.pikachu;
            enemy = Meowsta_dictionary.meowleaf;
          };
      preload_textures state.renderer state.texture_table)
    else ();
    player := (Option.get state.battle_state).player;
    enemy := (Option.get state.battle_state).enemy

  let update state =
    if !player_move = None then ()
    else
      let result = run_turn () in
      if result = 0 then (
        print_endline "You win!";
        state.action_state <- Roaming)
      else if result = 1 then (
        print_endline "You lose :(";
        state.action_state <- Roaming)
      else ();
      player_move := None

  let render_pokemon state sprite_name = function
    | x, y ->
        let dst = Sdl.Rect.create ~x ~y ~w:300 ~h:300 in
        let _ =
          Sdl.render_copy ~dst state.renderer
            (Hashtbl.find state.texture_table sprite_name)
        in
        ()

  let render state =
    let renderer = state.renderer in
    let _ = Sdl.render_clear renderer in
    let _ =
      Sdl.render_copy renderer
        (Hashtbl.find state.texture_table background_file)
    in
    render_pokemon state !player.name pos1;
    render_pokemon state !enemy.name pos2;
    Sdl.render_present renderer

  let handle_events state =
    let e = Sdl.Event.create () in
    while Sdl.poll_event (Some e) do
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> state.is_running <- false
      | `Key_down ->
          let key = Sdl.Event.(get e keyboard_keycode) in
          if key = Sdl.K.m then (
            state.action_state <- Menu;
            state.previous_state <- Battle)
          else if key = Sdl.K.space then player_move := Some !player.moveset.(0)
      | _ -> ()
    done
end
