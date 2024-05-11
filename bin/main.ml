open Tsdl
open Tsdl_image
open Meowstas
open Tile
open Tilemaps

let window_size = ref (1920, 1056)
let tile_size = ref 96
let texture_table = Hashtbl.create 20

let load_texture_from_file renderer file =
  match Image.load file with
  | Error (`Msg e) ->
      Sdl.log "Failed to load %s: %s" file e;
      exit 1
  | Ok surface -> (
      match Sdl.create_texture_from_surface renderer surface with
      | Error (`Msg e) ->
          Sdl.log "Create texture from surface error: %s" e;
          exit 1
      | Ok texture ->
          Sdl.free_surface surface;
          texture)

let preload_textures renderer =
  List.iter
    (fun tile ->
      let path = tile.texture in
      if not (Hashtbl.mem texture_table path) then
        let texture = load_texture_from_file renderer path in
        Hashtbl.add texture_table path texture)
    Tilemaps.tile_list

let resize_window window new_width new_height =
  Sdl.set_window_size window ~w:new_width ~h:new_height;
  window_size := (new_width, new_height)

let game_map = Map.make () "begin" beginmap (20, 15)

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

let render_player renderer game_map =
  let x, y = Map.get_player_pos game_map in
  let src = Sdl.Rect.create ~x:0 ~y:0 ~w:!tile_size ~h:!tile_size in
  let dst =
    Sdl.Rect.create
      ~x:(extract_int x * !tile_size)
      ~y:(extract_int y * !tile_size)
      ~w:!tile_size ~h:!tile_size
  in
  let player = Map.get_player game_map in
  let texture =
    match player.state with
    | South -> load_texture_from_file renderer "textures/pc-front1.png"
    | North -> load_texture_from_file renderer "textures/pc-back1.png"
    | West -> load_texture_from_file renderer "textures/pc-left1.png"
    | East -> load_texture_from_file renderer "textures/pc-right1.png"
  in
  match Sdl.render_copy ~src ~dst renderer texture with
  | Ok () -> () (* Successfully rendered, do nothing *)
  | Error (`Msg e) -> Sdl.log "Failed to render tile: %s" e

let handle_events map =
  let e = Sdl.Event.create () in
  let continue = ref true in
  while Sdl.poll_event (Some e) do
    match Sdl.Event.(enum (get e typ)) with
    | `Quit ->
        continue := false;
        Printf.printf "window quit"
    | `Key_down -> (
        let key = Sdl.Event.(get e keyboard_keycode) in
        match key with
        | 0x77 ->
            Map.update_location map Up;
            Printf.printf "W key down"
        | 0x61 ->
            Map.update_location map Left;
            Printf.printf "A key down"
        | 0x73 ->
            Map.update_location map Down;
            Printf.printf "S key down"
        | 0x64 ->
            Map.update_location map Right;
            Printf.printf "D key down"
        | _ -> ())
    | _ -> ()
  done;
  !continue

let render_map renderer (map : Map.map) =
  let w, h = !window_size in
  let tile_size = w / Array.length (Map.get_grid map) in
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j tile -> render_tile renderer j i tile texture_table)
        row)
    (Map.get_grid map)

let main () =
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
                render_map renderer game_map;
                render_player renderer game_map;
                Sdl.render_present renderer;
                (* let pos = Map.get_player_pos game_map in Printf.printf "%d,
                   %d\n" (extract_int (fst pos)) (extract_int (snd pos)); *)
                match handle_events game_map with
                | true ->
                    let get_ticks = Sdl.get_ticks () in
                    let elapsed_ticks = Int32.sub get_ticks start_ticks in
                    let frame_delay = Int32.sub 33l elapsed_ticks in
                    (* Approximately 30 FPS *)
                    Sdl.pump_events ();
                    if frame_delay > 0l then Sdl.delay frame_delay;
                    loop ();
                    ()
                | false ->
                    Sdl.destroy_window w;
                    Sdl.quit ()
              in
              Map.create_player game_map (Some 4, Some 4);
              loop ();
              Sdl.destroy_window w;
              Sdl.quit ()))

let () =
  ignore (Sdl.init Sdl.Init.everything);
  let flags = Image.Init.(jpg + png) in
  assert (Image.(Init.test (init flags) Init.png));
  main ()
