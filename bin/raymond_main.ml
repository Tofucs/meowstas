open Tsdl
open Meowstas
open Tile
open Tsdl_image
open Tsdl_ttf
open Main_utils

module BattleState = struct
  type t = global_state option

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

  let window = ref None
  let renderer = ref None
  let is_running = ref false

  let init =
    let _ = Ttf.init () in
    let _ = Sdl.init Sdl.Init.everything in
    let window = ref None in
    let renderer = ref None in
    match Sdl.create_window ~w:1920 ~h:1080 "Battle" Sdl.Window.opengl with
    | Error (`Msg e) ->
        Sdl.log "Create window error: %s" e;
        exit 1
    | Ok w ->
        (window := Some w;
         match
           Sdl.create_renderer w ~index:(-1) ~flags:Sdl.Renderer.accelerated
         with
         | Error (`Msg e) ->
             Sdl.log "Create renderer error: %s" e;
             exit 1
         | Ok r ->
             renderer := Some r;
             let _ = Sdl.set_render_draw_color r 255 0 0 255 in
             is_running := true);
        Some
          {
            renderer = Option.get !renderer;
            window = Option.get !window;
            window_size = (1920, 1080);
            action_state = Roaming;
            world = World.get_instance ();
          }

  let update (state : global_state) = ()

  let render (state : global_state) =
    let _ = Sdl.render_clear (Option.get !renderer) in
    Sdl.render_present (Option.get !renderer)

  let on_destroy () =
    Sdl.destroy_renderer (Option.get !renderer);
    Sdl.destroy_window (Option.get !window);
    Sdl.quit ();
    print_endline "Quit Application"

  let handle_events (state : global_state) =
    let e = Sdl.Event.create () in
    while Sdl.poll_event (Some e) do
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> is_running := false
      | _ -> ()
    done
end
