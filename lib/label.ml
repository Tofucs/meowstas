open Tsdl
open Tsdl_ttf

(* Define the button type including the callback function and its parameter *)
type t = {
  mutable rect : Sdl.rect;
  mutable text : string;
  mutable texture : Sdl.texture;
  renderer : Sdl.renderer;
  font_size : int;
  font_color : Sdl.color;
}

(* Function to create a button *)
let create ?(font_size = 35)
    ?(font_color = Sdl.Color.create ~r:255 ~g:255 ~b:255 ~a:255) renderer ~x ~y
    ~text =
  Ttf.init () |> ignore;
  let font =
    Ttf.open_font "lib/Roboto-Regular.ttf" font_size |> Result.get_ok
  in

  (* Create texture from text if no texture is provided *)
  let surface =
    Ttf.render_text_blended_wrapped font text font_color 300l |> Result.get_ok
  in
  let texture =
    Sdl.create_texture_from_surface renderer surface |> Result.get_ok
  in
  Sdl.free_surface surface;
  Sdl.query_texture texture |> Result.get_ok |> fun (_, _, (w, h)) ->
  {
    rect = Sdl.Rect.create ~x ~y ~w ~h;
    text;
    texture;
    renderer;
    font_size;
    font_color;
  }

let update_texture label new_text =
  label.text <- new_text;
  let font =
    Ttf.open_font "lib/Roboto-Regular.ttf" label.font_size |> Result.get_ok
  in

  (* Create texture from text if no texture is provided *)
  let surface =
    Ttf.render_text_blended_wrapped font new_text label.font_color 600l
    |> Result.get_ok
  in
  let texture =
    Sdl.create_texture_from_surface label.renderer surface |> Result.get_ok
  in
  Sdl.free_surface surface;
  Sdl.query_texture texture |> Result.get_ok |> fun (_, _, (w, h)) ->
  label.rect <-
    Sdl.Rect.create ~x:(Sdl.Rect.x label.rect) ~y:(Sdl.Rect.y label.rect) ~w ~h;
  label.texture <- texture

(* Function to set the text of the button *)

let truncate string =
  print_string string;
  let lines = String.split_on_char '\n' string in
  let lines = List.rev lines in
  print_string (List.nth lines 1);
  List.nth lines 4 ^ "\n" ^ List.nth lines 3 ^ "\n" ^ List.nth lines 2 ^ "\n"
  ^ List.nth lines 1 ^ "\n" ^ List.nth lines 0

let add_text label string =
  update_texture label (truncate (label.text ^ string))

(* Function to render the button *)
let render { rect; texture; renderer; _ } =
  Sdl.render_copy renderer ~dst:rect texture |> ignore
(* Sdl.set_render_draw_color renderer 0 0 0 255 |> ignore *)
