open Tsdl
open Meowstas
open Tile
open Tsdl_image
open Tsdl_ttf

let load_texture_from_file renderer file =
  match Sdl.load_bmp file with
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

let extract_int opt =
  match opt with
  | Some i -> i
  | None -> 0

let render_player renderer game_map =
  let x, y = Map.get_player_pos game_map in
  let src = Sdl.Rect.create ~x:0 ~y:0 ~w:32 ~h:32 in
  let dst =
    Sdl.Rect.create ~x:(extract_int x * 32) ~y:(extract_int y * 32) ~w:32 ~h:32
  in
  let player = Map.get_player game_map in
  let texture =
    match player.state with
    | South -> load_texture_from_file renderer "textures/pc-front1.bmp"
    | North -> load_texture_from_file renderer "textures/pc-back1.bmp"
    | West -> load_texture_from_file renderer "textures/pc-left1.bmp"
    | East -> load_texture_from_file renderer "textures/pc-right1.bmp"
  in
  match Sdl.render_copy ~src ~dst renderer texture with
  | Ok () -> () (* Successfully rendered, do nothing *)
  | Error (`Msg e) -> Sdl.log "Failed to render tile: %s" e

let window = ref None
let renderer = ref None
let is_running = ref false

let init () =
  match Sdl.init Sdl.Init.everything with
  | Error (`Msg e) ->
      Sdl.log "Init error: %s" e;
      exit 1
  | Ok () -> (
      match Sdl.create_window ~w:1280 ~h:720 "Battle" Sdl.Window.opengl with
      | Error (`Msg e) ->
          Sdl.log "Create window error: %s" e;
          exit 1
      | Ok w -> (
          window := Some w;
          match
            Sdl.create_renderer w ~index:(-1) ~flags:Sdl.Renderer.accelerated
          with
          | Error (`Msg e) ->
              Sdl.log "Create renderer error: %s" e;
              exit 1
          | Ok r ->
              renderer := Some r;
              let _ = Sdl.set_render_draw_color r 255 0 0 255 in
              is_running := true))

let update () = ()

let render () =
  let _ = Sdl.render_clear (Option.get !renderer) in
  Sdl.render_present (Option.get !renderer)

let on_destroy () =
  Sdl.destroy_renderer (Option.get !renderer);
  Sdl.destroy_window (Option.get !window);
  Sdl.quit ();
  print_endline "Quit Application"

let handle_events () =
  let e = Sdl.Event.create () in
  while Sdl.poll_event (Some e) do
    match Sdl.Event.(enum (get e typ)) with
    | `Quit -> is_running := false
    | _ -> ()
  done

let main () =
  init ();
  while !is_running do
    handle_events ();
    update ();
    render ()
  done;
  on_destroy ()

let () = main ()
let _ = Ttf.init ()
