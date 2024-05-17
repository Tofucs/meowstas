open Tsdl
open Tile
open Tsdl_image
open Tsdl_ttf
open Main_utils

module RoamingMode : GameMode = struct
  type t = global_state

  let tile_size = ref 96
  let () = Random.self_init ()

  let preload_textures renderer texture_table =
    List.iter
      (fun tile ->
        let path = tile.texture in
        if not (Hashtbl.mem texture_table path) then
          let texture = load_texture_from_png renderer path in
          Hashtbl.add texture_table path texture)
      Tilemaps.tile_list

  let render_tile renderer x y tile texture_table =
    let src = Sdl.Rect.create ~x:0 ~y:0 ~w:!tile_size ~h:!tile_size in
    let dst =
      Sdl.Rect.create ~x:(x * !tile_size) ~y:(y * !tile_size) ~w:!tile_size
        ~h:!tile_size
    in
    let texture = Hashtbl.find texture_table tile.texture in
    match Sdl.render_copy ~src ~dst renderer texture with
    | Ok () -> () (* Successfully rendered, do nothing *)
    | Error (`Msg e) -> Sdl.log "Failed to render tile: %s" e

  let extract_int opt =
    match opt with
    | Some i -> i
    | None -> 0

  let interpolate_position start_ticks current_ticks (start_x, start_y)
      (end_x, end_y) duration tile_size =
    let elapsed_time = current_ticks - start_ticks in
    let progress =
      min (float_of_int elapsed_time /. float_of_int duration) 1.0
    in

    let start_pixel_x = start_x * tile_size in
    let start_pixel_y = start_y * tile_size in
    let end_pixel_x = end_x * tile_size in
    let end_pixel_y = end_y * tile_size in

    let interpolated_x =
      float_of_int start_pixel_x
      +. (progress *. float_of_int (end_pixel_x - start_pixel_x))
    in
    let interpolated_y =
      float_of_int start_pixel_y
      +. (progress *. float_of_int (end_pixel_y - start_pixel_y))
    in

    (interpolated_x, interpolated_y)

  let start_animation game map (direction : Player.moves) =
    let xp, yp = Map.get_player_pos map in
    let x, y = (extract_int xp, extract_int yp) in
    let dx, dy =
      match direction with
      | Up -> (0, -1)
      | Down -> (0, 1)
      | Left -> (-1, 0)
      | Right -> (1, 0)
      | _ -> (0, 0)
    in
    game.start_pos <- (x - dx, y - dy);
    game.end_pos <- (x, y);
    game.animation_start <- Int32.to_int (Sdl.get_ticks ());
    game.animation_duration <- 250;
    game.in_animation <- true

  let update_animation game =
    let current_ticks = Int32.to_int (Sdl.get_ticks ()) in
    if current_ticks - game.animation_start >= game.animation_duration - 50 then begin
      game.in_animation <- false
    end

  let render_player renderer game_map game_state =
    let current_ticks = Int32.to_int (Sdl.get_ticks ()) in
    let render_x, render_y =
      if game_state.in_animation then
        interpolate_position game_state.animation_start current_ticks
          game_state.start_pos game_state.end_pos game_state.animation_duration
          !tile_size
      else
        let x, y = Map.get_player_pos game_map in
        ( float_of_int (extract_int x * !tile_size),
          float_of_int (extract_int y * !tile_size) )
    in
    let src = Sdl.Rect.create ~x:0 ~y:0 ~w:!tile_size ~h:!tile_size in
    let dst =
      Sdl.Rect.create ~x:(int_of_float render_x) ~y:(int_of_float render_y)
        ~w:!tile_size ~h:!tile_size
    in
    let player = Map.get_player game_map in
    let texture =
      match player.state with
      | South -> load_texture_from_png renderer "textures/pc-front-96.png"
      | North -> load_texture_from_png renderer "textures/pc-back-96.png"
      | West -> load_texture_from_png renderer "textures/pc-left-96.png"
      | East -> load_texture_from_png renderer "textures/pc-right-96.png"
    in
    match Sdl.render_copy ~src ~dst renderer texture with
    | Ok () -> () (* Successfully rendered, do nothing *)
    | Error (`Msg e) -> Sdl.log "Failed to render tile: %s" e

  let render_map renderer (map : Map.map) texture_table =
    Array.iteri
      (fun i row ->
        Array.iteri
          (fun j tile -> render_tile renderer j i tile texture_table)
          row)
      (Map.get_grid map)

  let render_deco renderer (map : Map.map) =
    let grid = Map.get_grid map in
    Array.iteri
      (fun y row ->
        Array.iteri
          (fun x tile ->
            if tile.deco1 then
              let tree_texture =
                load_texture_from_png renderer "textures/tree1.png"
              in
              let src =
                Sdl.Rect.create ~x:0 ~y:0 ~w:!tile_size ~h:(2 * !tile_size)
              in
              let dst =
                Sdl.Rect.create ~x:(x * !tile_size)
                  ~y:((y * !tile_size) - !tile_size)
                  ~w:!tile_size ~h:(2 * !tile_size)
              in
              match Sdl.render_copy ~src ~dst renderer tree_texture with
              | Ok () -> () (* Successfully rendered, do nothing *)
              | Error (`Msg e) -> Sdl.log "Failed to render decoration: %s" e)
          row)
      grid

  let fade_in renderer duration window_size =
    let start_time = Sdl.get_ticks () in
    let rect =
      Sdl.Rect.create ~x:0 ~y:0 ~w:(fst window_size) ~h:(snd window_size)
    in
    for alpha = 0 to 255 do
      let elapsed = Int32.to_int (Int32.sub (Sdl.get_ticks ()) start_time) in
      if elapsed < duration then (
        let progress = elapsed * 255 / duration in
        ignore (Sdl.set_render_draw_color renderer 0 0 0 progress);
        ignore (Sdl.render_fill_rect renderer (Some rect));
        Sdl.render_present renderer;
        Sdl.delay 5l)
    done

  let fade_out renderer duration window_size =
    let start_time = Sdl.get_ticks () in
    let rect =
      Sdl.Rect.create ~x:0 ~y:0 ~w:(fst window_size) ~h:(snd window_size)
    in
    for alpha = 255 to 0 do
      let elapsed = Int32.to_int (Int32.sub (Sdl.get_ticks ()) start_time) in
      if elapsed < duration then (
        let progress = elapsed * 255 / duration in
        ignore (Sdl.set_render_draw_color renderer 0 0 0 progress);
        ignore (Sdl.render_fill_rect renderer (Some rect));
        Sdl.render_present renderer;
        Sdl.delay 5l)
    done

  let fade_transition renderer duration window_size =
    fade_in renderer duration window_size;
    fade_out renderer duration window_size

  let init state =
    if state.roaming_state = None then (
      World.initialize ();
      let world = World.get_instance () in
      state.roaming_state <-
        Some
          {
            world;
            in_animation = false;
            in_transition = false;
            player_location = (4, 4);
            animation_start = 0;
            animation_duration = 0;
            start_pos = (0, 0);
            end_pos = (0, 0);
          };
      let _ =
        Sdl.set_render_draw_blend_mode state.renderer Sdl.Blend.mode_blend
      in
      preload_textures state.renderer state.texture_table;
      Map.create_player (World.get_map world.current_location) (Some 4, Some 4))
    else
      let world = World.get_instance () in
      let _ =
        Sdl.set_render_draw_blend_mode state.renderer Sdl.Blend.mode_blend
      in
      let roam = Option.get state.roaming_state in
      let loc = roam.player_location in
      preload_textures state.renderer state.texture_table;
      Map.create_player
        (World.get_map world.current_location)
        (Some (fst loc), Some (snd loc))

  let random_encounter () = Random.int 10 > 7
  let update state = ()

  let handle_events state =
    let game_state = Option.get state.roaming_state in
    let map = World.get_map game_state.world.current_location in
    let prev_loc = game_state.world.current_location in
    let e = Sdl.Event.create () in
    while Sdl.poll_event (Some e) do
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> state.is_running <- false
      | `Key_down -> (
          if not game_state.in_animation then
            let key = Sdl.Event.(get e keyboard_keycode) in
            match key with
            | 0x77 ->
                if
                  Map.update_location map Up World.update_map
                  && prev_loc = game_state.world.current_location
                then (
                  start_animation game_state map Up;
                  game_state.player_location <-
                    ( fst game_state.player_location,
                      snd game_state.player_location - 1 );
                  begin
                    if random_encounter () then
                      match
                        (Map.query_tile map (Map.get_player_pos map)).interact
                      with
                      | IW Encounter -> state.action_state <- Battle
                      | _ -> ()
                  end)
            | 0x61 ->
                if
                  Map.update_location map Left World.update_map
                  && prev_loc = game_state.world.current_location
                then (
                  start_animation game_state map Left;
                  game_state.player_location <-
                    ( fst game_state.player_location - 1,
                      snd game_state.player_location );
                  begin
                    if random_encounter () then
                      match
                        (Map.query_tile map (Map.get_player_pos map)).interact
                      with
                      | IW Encounter -> state.action_state <- Battle
                      | _ -> ()
                  end)
            | 0x73 ->
                if
                  Map.update_location map Down World.update_map
                  && prev_loc = game_state.world.current_location
                then (
                  start_animation game_state map Down;
                  game_state.player_location <-
                    ( fst game_state.player_location,
                      snd game_state.player_location + 1 );
                  begin
                    if random_encounter () then
                      match
                        (Map.query_tile map (Map.get_player_pos map)).interact
                      with
                      | IW Encounter -> state.action_state <- Battle
                      | _ -> ()
                  end)
            | 0x64 ->
                if
                  Map.update_location map Right World.update_map
                  && prev_loc = game_state.world.current_location
                then (
                  start_animation game_state map Right;
                  game_state.player_location <-
                    ( fst game_state.player_location + 1,
                      snd game_state.player_location );
                  begin
                    if random_encounter () then
                      match
                        (Map.query_tile map (Map.get_player_pos map)).interact
                      with
                      | IW Encounter -> state.action_state <- Battle
                      | _ -> ()
                  end)
            | _ -> ())
      | _ -> ()
    done

  let render state =
    let renderer = state.renderer in
    let game = Option.get state.roaming_state in
    let _ = Sdl.render_clear renderer in
    update_animation game;
    render_map renderer
      (World.get_map game.world.current_location)
      state.texture_table;
    render_player renderer (World.get_map game.world.current_location) game;
    render_deco renderer (World.get_map game.world.current_location);
    Sdl.render_present renderer;
    (* let pos = Map.get_player_pos game_map in Printf.printf "%d, %d\n"
       (extract_int (fst pos)) (extract_int (snd pos)); *)
    (* Printf.printf "%s\n" game.world.current_location; *)
    (* Printf.printf "%d|%d\n" (Option.get (fst (Map.get_player_pos
       (World.get_map game.world.current_location)))) (Option.get (snd
       (Map.get_player_pos (World.get_map game.world.current_location)))); *)
    let prev_loc = game.world.current_location in
    handle_events state;
    if prev_loc <> game.world.current_location then game.in_transition <- true;
    if game.in_transition then fade_transition renderer 1000 state.window_size;
    game.in_transition <- false
end
