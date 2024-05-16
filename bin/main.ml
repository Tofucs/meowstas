open Tsdl
open Meowstas
open Tsdl_image
open Tsdl_ttf
open Button
open Main_utils
open Battle_mode
open Roaming_mode
open Menu_mode

(* let previous_state = ref None *)

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
        menu_state = None;
        battle_state = None;
        roaming_state = None;
        texture_table = Hashtbl.create 20;
        previous_state = Roaming;
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
  RoamingMode.update state;
  RoamingMode.render state

let run_menu_mode state =
  MenuMode.handle_events state;
  (* MenuMode.update state; *)
  MenuMode.render state

let handle_events state =
  let e = Sdl.Event.create () in
  while Sdl.poll_event (Some e) do
    match Sdl.Event.(enum (get e typ)) with
    | `Key_down -> (
        Printf.printf "keydown";
        let key = Sdl.Event.(get e keyboard_keycode) in
        if key = Sdl.K.m then
          (* Assuming K.space is the keycode for space *)
          match state.action_state with
          | Menu ->
              (* If we are in the menu, switch back to the previous state *)
              state.action_state <- state.previous_state
              (* No previous state, perhaps do nothing or set a default *)
          | _ ->
              (* If we are not in the menu, switch to it and remember the
                 current state *)
              state.previous_state <- state.action_state;
              state.action_state <- Menu)
    | _ -> ()
  done

let main () =
  let game = init () in
  while game.is_running do
    let start_ticks = Sdl.get_ticks () in
    (if game.action_state = Battle then (
       match game.battle_state with
       | Some _ ->
           handle_events game;
           run_battle_mode game
       | None ->
           handle_events game;
           BattleMode.init game;
           run_battle_mode game)
     else if game.action_state = Menu then (
       match game.menu_state with
       | Some _ ->
           run_menu_mode game;
           handle_events game
       | None ->
           MenuMode.init game;
           run_menu_mode game;
           handle_events game)
     else
       match game.roaming_state with
       | Some _ ->
           handle_events game;
           run_roaming_mode game
       | None ->
           handle_events game;
           RoamingMode.init game;
           run_roaming_mode game);

    let get_ticks = Sdl.get_ticks () in
    let elapsed_ticks = Int32.sub get_ticks start_ticks in
    let frame_delay = Int32.sub 17l elapsed_ticks in
    (* Approximately 60 FPS *)
    Sdl.pump_events ();
    (* Assuming you're polling events like this *)
    if frame_delay > 0l then Sdl.delay frame_delay
  done;
  on_destroy game

let () = main ()
