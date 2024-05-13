open Tsdl
open Tsdl_image
open Meowstas
open Tile
open Tilemaps
open Main_utils

type action_state =
  | Roaming
  | Battle

type game_state = {
  mutable current_state : action_state;
  world : World.world;
  mutable in_animation : bool;
  mutable in_transition : bool;
  mutable animation_start : int;
  mutable animation_duration : int;
  mutable start_pos : int * int;
  mutable end_pos : int * int;
}

let window_size = ref (1920, 1056)
let tile_size = ref 96
let texture_table = Hashtbl.create 20

let preload_textures renderer =
  List.iter
    (fun tile ->
      let path = tile.texture in
      if not (Hashtbl.mem texture_table path) then
        let texture = load_texture_from_png renderer path in
        Hashtbl.add texture_table path texture)
    Tilemaps.tile_list

let resize_window window new_width new_height =
  Sdl.set_window_size window ~w:new_width ~h:new_height;
  window_size := (new_width, new_height)

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
  let progress = min (float_of_int elapsed_time /. float_of_int duration) 1.0 in

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

let handle_events map game_state prev_loc =
  let e = Sdl.Event.create () in
  let continue = ref true in
  while Sdl.poll_event (Some e) do
    match Sdl.Event.(enum (get e typ)) with
    | `Quit ->
        continue := false;
        Printf.printf "window quit"
    | `Key_down -> (
        if not game_state.in_animation then
          let key = Sdl.Event.(get e keyboard_keycode) in
          match key with
          | 0x77 ->
              if
                Map.update_location map Up World.update_map
                && prev_loc = game_state.world.current_location
              then start_animation game_state map Up
          | 0x61 ->
              if
                Map.update_location map Left World.update_map
                && prev_loc = game_state.world.current_location
              then start_animation game_state map Left
          | 0x73 ->
              if
                Map.update_location map Down World.update_map
                && prev_loc = game_state.world.current_location
              then start_animation game_state map Down
          | 0x64 ->
              if
                Map.update_location map Right World.update_map
                && prev_loc = game_state.world.current_location
              then start_animation game_state map Right
          | _ -> ())
    | _ -> ()
  done;
  !continue

let render_map renderer (map : Map.map) =
  let w, h = !window_size in
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

let fade_in renderer duration =
  let start_time = Sdl.get_ticks () in
  let rect =
    Sdl.Rect.create ~x:0 ~y:0 ~w:(fst !window_size) ~h:(snd !window_size)
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

let fade_out renderer duration =
  let start_time = Sdl.get_ticks () in
  let rect =
    Sdl.Rect.create ~x:0 ~y:0 ~w:(fst !window_size) ~h:(snd !window_size)
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

let fade_transition renderer duration =
  fade_in renderer duration;
  fade_out renderer duration

let main game () =
  match Sdl.init Sdl.Init.(video + events) with
  | Error (`Msg e) ->
      Sdl.log "Init error: %s" e;
      exit 1
  | Ok () -> (
      match
        Sdl.create_window ~w:(fst !window_size) ~h:(snd !window_size)
          "SDL OpenGL" Sdl.Window.opengl
      with
      | Error (`Msg e) ->
          Sdl.log "Create window error: %s" e;
          exit 1
      | Ok w -> (
          match
            Sdl.create_renderer w ~index:(-1) ~flags:Sdl.Renderer.accelerated
          with
          | Error (`Msg e) ->
              Sdl.log "Create renderer error: %s" e;
              exit 1
          | Ok renderer ->
              ignore
                (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);
              preload_textures renderer;
              let rec loop () =
                let start_ticks = Sdl.get_ticks () in
                update_animation game;
                render_map renderer (World.get_map game.world.current_location);
                render_player renderer
                  (World.get_map game.world.current_location)
                  game;
                render_deco renderer (World.get_map game.world.current_location);
                Sdl.render_present renderer;
                (* let pos = Map.get_player_pos game_map in Printf.printf "%d,
                   %d\n" (extract_int (fst pos)) (extract_int (snd pos)); *)
                Printf.printf "%s" game.world.current_location;
                let prev_loc = game.world.current_location in
                match
                  handle_events
                    (World.get_map game.world.current_location)
                    game game.world.current_location
                with
                | true ->
                    if prev_loc <> game.world.current_location then
                      game.in_transition <- true;
                    if game.in_transition then fade_transition renderer 1000;
                    game.in_transition <- false;
                    let get_ticks = Sdl.get_ticks () in
                    let elapsed_ticks = Int32.sub get_ticks start_ticks in
                    let frame_delay = Int32.sub 17l elapsed_ticks in
                    (* Approximately 60 FPS *)
                    Sdl.pump_events ();
                    if frame_delay > 0l then Sdl.delay frame_delay;
                    loop ();
                    ()
                | false ->
                    Sdl.destroy_window w;
                    Sdl.quit ()
              in
              Map.create_player
                (World.get_map game.world.current_location)
                (Some 4, Some 4);
              loop ();
              Sdl.destroy_window w;
              Sdl.quit ()))

let () =
  ignore (Sdl.init Sdl.Init.everything);
  let flags = Image.Init.(jpg + png) in
  assert (Image.(Init.test (init flags) Init.png));
  World.initialize ();
  let world = World.get_instance () in
  let game : game_state =
    {
      current_state = Roaming;
      world;
      in_animation = false;
      in_transition = false;
      animation_start = 0;
      animation_duration = 0;
      start_pos = (0, 0);
      end_pos = (0, 0);
    }
  in
  main game ()
