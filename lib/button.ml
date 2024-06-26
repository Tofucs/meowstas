open Tsdl
open Tsdl_image
open Tsdl_ttf
open Items
open Meowsta

(* Define the button type including the callback function and its parameter *)
type t = {
  rect : Sdl.rect;
  text : string;
  texture : Sdl.texture;
  renderer : Sdl.renderer;
  item : items;
  meowsta : Meowsta.t;
}

(* Function to create a button *)
let create ?(font_size = 35)
    ?(font_color = Sdl.Color.create ~r:255 ~g:0 ~b:0 ~a:255) renderer ~x ~y
    ~(w : int) ~(h : int) ~text ~item ~meowsta =
  Ttf.init () |> ignore;
  let font =
    Ttf.open_font "lib/Roboto-Regular.ttf" font_size |> Result.get_ok
  in

  (* Create texture from text if no texture is provided *)
  let surface = Ttf.render_text_blended font text font_color |> Result.get_ok in
  let texture =
    Sdl.create_texture_from_surface renderer surface |> Result.get_ok
  in
  Sdl.free_surface surface;
  Sdl.query_texture texture |> Result.get_ok |> fun (_, _, (w, h)) ->
  { rect = Sdl.Rect.create ~x ~y ~w ~h; text; texture; renderer; item; meowsta }

(* Function to render the button *)
let render { rect; texture; renderer; _ } =
  Sdl.set_render_draw_color renderer 255 255 255 255 |> ignore;
  (* White color *)
  (* Sdl.render_fill_rect renderer (Some rect) |> ignore; *)
  Sdl.render_copy renderer ~dst:rect texture |> ignore
(* Sdl.set_render_draw_color renderer 0 0 0 255 |> ignore *)
