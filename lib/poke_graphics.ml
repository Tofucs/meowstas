open Graphics

let width = 800
let height = 700
let background_color = rgb 255 255 255
let text_color = black
let option_color = rgb 128 128 128
let font = "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1"

let init () =
  open_graph (Printf.sprintf " %dx%d" width height);
  set_window_title "Battle Screen";
  set_color background_color;
  fill_rect 0 0 width height;
  set_font font

let draw_centered_string x y text =
  let text_width, text_height = text_size text in
  moveto (x - (text_width / 2)) (y - (text_height / 2));
  draw_string text

let draw_interface pokemon =
  set_color text_color;
  set_line_width 8;
  draw_rect 0 0 (size_x () - 1) (size_y () - 1);
  moveto 0 (height / 3);
  set_line_width 3;
  lineto width (height / 3);
  let poke_string = "What will " ^ pokemon ^ " do?" in
  draw_centered_string (width / 2) ((height / 3) - 50) poke_string;
  set_color option_color;
  let option_height = 40 in
  let option_width = (width / 2) - 20 in
  fill_rect 10 ((height / 3) - 90 - option_height) option_width option_height;
  fill_rect
    ((width / 2) + 10)
    ((height / 3) - 90 - option_height)
    option_width option_height;
  fill_rect 10
    ((height / 3) - 140 - (2 * option_height))
    option_width option_height;
  fill_rect
    ((width / 2) + 10)
    ((height / 3) - 140 - (2 * option_height))
    option_width option_height;
  set_color text_color;
  draw_centered_string (width / 4) ((height / 3) - 110) "FIGHT";
  draw_centered_string (3 * width / 4) ((height / 3) - 110) "BAG";
  draw_centered_string (width / 4) ((height / 3) - 200) "POKEMON";
  draw_centered_string (3 * width / 4) ((height / 3) - 200) "RUN"

let close () =
  ignore (read_key ());
  close_graph ()
