open Tsdl
open Meowstas
open Tile

type textures = {
  w : Sdl.texture;
  nw : Sdl.texture;
  iw : Sdl.texture;
  inw : Sdl.texture;
}

let beginmap =
  let tiles =
    [
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
    ]
  in
  let convert_row row = Array.of_list row in
  Array.of_list (List.map convert_row tiles)

let game_map = Map.make () "begin" beginmap (20, 15)

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

let render_tile renderer textures x y tile =
  let src = Sdl.Rect.create ~x:0 ~y:0 ~w:32 ~h:32 in
  let dst = Sdl.Rect.create ~x:(x * 32) ~y:(y * 32) ~w:32 ~h:32 in
  let texture =
    match tile with
    | W -> textures.w
    | NW -> textures.nw
    | IW -> textures.iw
    | INW -> textures.inw
  in
  match Sdl.render_copy ~src ~dst renderer texture with
  | Ok () -> () (* Successfully rendered, do nothing *)
  | Error (`Msg e) -> Sdl.log "Failed to render tile: %s" e

let handle_events map =
  let e = Sdl.Event.create () in
  let continue = ref true in
  while Sdl.poll_event (Some e) do
    match Sdl.Event.(enum (get e typ)) with
    | `Quit -> continue := false
    | `Key_down -> (
        let key = Sdl.Event.(get e keyboard_keycode) in
        match key with
        | 0x77 -> Map.update_location map Up
        | 0x61 -> Map.update_location map Down
        | 0x73 -> Map.update_location map Left
        | 0x64 -> Map.update_location map Right
        | _ -> ())
    | _ -> ()
  done;
  !continue

let render_map renderer textures (map : Map.map) =
  Array.iteri
    (fun i row ->
      Array.iteri (fun j tile -> render_tile renderer textures j i tile) row)
    (Map.get_grid map)

let main () =
  match Sdl.init Sdl.Init.(video + events) with
  | Error (`Msg e) ->
      Sdl.log "Init error: %s" e;
      exit 1
  | Ok () -> (
      match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
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
              let textures =
                {
                  w = load_texture_from_file renderer "textures/sand1.bmp";
                  nw = load_texture_from_file renderer "textures/ocean1.bmp";
                  iw = load_texture_from_file renderer "textures/sand1.bmp";
                  inw = load_texture_from_file renderer "textures/ocean1.bmp";
                }
              in
              let rec loop () =
                let start_ticks = Sdl.get_ticks () in
                render_map renderer textures game_map;
                Sdl.render_present renderer;
                let get_ticks = Sdl.get_ticks () in
                let elapsed_ticks = Int32.sub get_ticks start_ticks in
                let frame_delay = Int32.sub 33l elapsed_ticks in
                (* Approximately 30 FPS *)
                if frame_delay > 0l then Sdl.delay frame_delay;
                loop ();
                ()
              in
              loop ();
              Sdl.destroy_window w;
              Sdl.quit ()))

let () = main ()
