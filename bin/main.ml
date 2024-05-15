open Tsdl
open Meowstas
open Tsdl_image
open Tsdl_ttf
open Button

let window = ref None
let renderer = ref None
let is_running = ref false

(* try to create a button class / module *)
(* don't worry about doing anything with the button, as long as it is
   pressable *)

let buttons_visible = ref false
let button = ref None
let button2 = ref None

(*Reference to store the button *)

let button_action param =
  print_endline ("Button pressed with param: " ^ string_of_int param)

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
              let btn =
                Button.create r ~x:100 ~y:100 ~w:150 ~h:50 ~text:"Click me!"
                  ?texture:None ~action:button_action ~action_param:123
              in
              button := Some btn;
              let btn2 =
                Button.create r ~x:500 ~y:100 ~w:150 ~h:50 ~text:"Click me!"
                  ?texture:None ~action:button_action ~action_param:123
              in
              button2 := Some btn2;
              let _ = Sdl.set_render_draw_color r 255 0 0 255 in
              is_running := true))

let update () = ()

let render () =
  let _ = Sdl.render_clear (Option.get !renderer) in
  (* write code here *)
  if !buttons_visible then begin
    Option.iter (fun btn -> Button.render btn) !button;
    Option.iter (fun btn -> Button.render btn) !button2
  end;

  Sdl.render_present (Option.get !renderer)

let on_destroy () =
  Option.iter Button.destroy !button;
  Sdl.destroy_renderer (Option.get !renderer);
  Sdl.destroy_window (Option.get !window);
  Sdl.quit ();
  print_endline "Quit Application"

let handle_events () =
  let e = Sdl.Event.create () in
  while Sdl.poll_event (Some e) do
    match Sdl.Event.(enum (get e typ)) with
    | `Quit -> is_running := false
    | `Key_down ->
        let key = Sdl.Event.(get e keyboard_keycode) in
        if key = Sdl.K.b then
          (* Assuming K.space is the keycode for space *)
          buttons_visible := not !buttons_visible
        (* Toggle visibility *)
    | _ ->
        if !buttons_visible then begin
          Option.iter (fun btn -> Button.handle_event btn e) !button;
          Option.iter (fun btn -> Button.handle_event btn e) !button2
        end
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
