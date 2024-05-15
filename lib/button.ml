open Tsdl
open Tsdl_image
open Tsdl_ttf

(* Define the button type including the callback function and its parameter *)
type t = {
  rect : Sdl.rect;
  text : string;
  texture : Sdl.texture;
  renderer : Sdl.renderer;
  action : int -> unit; (* The callback function taking an int parameter *)
  action_param : int; (* The parameter to pass to the callback *)
}

(* Function to create a button *)
let create renderer ~x ~y ~w ~h ~text ~action ~action_param ?texture =
  Ttf.init () |> ignore;
  let font = Ttf.open_font "lib/Roboto-Regular.ttf" 8 |> Result.get_ok in
  let final_texture =
    match texture with
    | Some tex -> tex (* Use provided texture *)
    | None ->
        (* Create texture from text if no texture is provided *)
        let surface =
          Ttf.render_text_solid font text
            (Sdl.Color.create ~r:255 ~g:0 ~b:0 ~a:255)
          |> Result.get_ok
        in
        let tex =
          Sdl.create_texture_from_surface renderer surface |> Result.get_ok
        in
        Sdl.free_surface surface;
        tex
  in
  {
    rect = Sdl.Rect.create ~x ~y ~w ~h;
    text;
    texture = final_texture;
    renderer;
    action;
    action_param;
  }

(* Function to render the button *)
let render { rect; texture; renderer; _ } =
  Sdl.set_render_draw_color renderer 255 255 255 255 |> ignore;
  (* White color *)
  (* Sdl.render_fill_rect renderer (Some rect) |> ignore; *)
  Sdl.render_copy renderer ~dst:rect texture |> ignore
(* Sdl.set_render_draw_color renderer 0 0 0 255 |> ignore *)

(* Function to check if the button is pressed *)
let is_pressed { rect; _ } ~mouse_x ~mouse_y =
  let rx = Sdl.Rect.x rect in
  let ry = Sdl.Rect.y rect in
  let rw = Sdl.Rect.w rect in
  let rh = Sdl.Rect.h rect in
  mouse_x >= rx && mouse_x <= rx + rw && mouse_y >= ry && mouse_y <= ry + rh

(* Handle button press *)
let handle_event button event =
  match Sdl.Event.(enum (get event typ)) with
  | `Mouse_button_down ->
      let x = Sdl.Event.(get event mouse_button_x) in
      let y = Sdl.Event.(get event mouse_button_y) in
      if is_pressed button ~mouse_x:x ~mouse_y:y then
        button.action button.action_param (* Execute the callback function *)
  | _ -> ()

(* Clean up resources *)
let destroy { texture; _ } = Sdl.destroy_texture texture
