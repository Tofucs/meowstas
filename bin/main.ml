open Tsdl
open Meowstas
open Tile
open Tsdl_image
open Tsdl_ttf
open Main_utils
open Battle_mode
open Roaming_mode

let init () =
  let _ = Sdl.init Sdl.Init.everything in
  let flags = Image.Init.(jpg + png) in
  assert (Image.(Init.test (init flags) Init.png));
  let _ = Ttf.init () in
  let window = ref None in
  let renderer = ref None in
  let window_size = (1920, 1056) in
  match
    Sdl.create_window ~w:(fst window_size) ~h:(snd window_size) "Battle"
      Sdl.Window.opengl
  with
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
           ());
      {
        is_running = true;
        renderer = Option.get !renderer;
        window = Option.get !window;
        window_size;
        action_state = Roaming;
        battle_state = None;
        roaming_state = None;
        texture_table = Hashtbl.create 20;
      }

let on_destroy state =
  Sdl.destroy_renderer state.renderer;
  Sdl.destroy_window state.window;
  Sdl.quit ();
  print_endline "Quit Application"

let run_battle_mode state =
  BattleMode.handle_events state;
  BattleMode.update state;
  BattleMode.render state

let run_roaming_mode state =
  RoamingMode.handle_events state;
  RoamingMode.update state;
  RoamingMode.render state

let main () =
  let singleton = init () in
  while singleton.is_running do
    let start_ticks = Sdl.get_ticks () in
    (if singleton.action_state = Battle then (
       match singleton.battle_state with
       | Some _ -> run_battle_mode singleton
       | None ->
           BattleMode.init singleton;
           run_battle_mode singleton)
     else
       match singleton.roaming_state with
       | Some _ -> run_roaming_mode singleton
       | None ->
           RoamingMode.init singleton;
           run_roaming_mode singleton);
    let get_ticks = Sdl.get_ticks () in
    let elapsed_ticks = Int32.sub get_ticks start_ticks in
    let frame_delay = Int32.sub 17l elapsed_ticks in
    (* Approximately 60 FPS *)
    Sdl.pump_events ();
    if frame_delay > 0l then Sdl.delay frame_delay
  done;
  on_destroy singleton

let () = main ()
