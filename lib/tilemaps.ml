open Tile

(*tiles*)

let grass_tile =
  {
    interact = W;
    terrain = Grass;
    deco1 = false;
    deco2 = false;
    texture = "textures/grass-normal.bmp";
  }

let grass_tile_alt =
  {
    interact = W;
    terrain = Grass;
    deco1 = false;
    deco2 = false;
    texture = "textures/grass-detail.bmp";
  }

let tall_grass_tile =
  {
    interact = IW Encounter;
    terrain = Grass;
    deco1 = true;
    deco2 = false;
    texture = "textures/grass-normal.bmp";
  }

let rock_grass_tile =
  {
    interact = NW;
    terrain = Grass;
    deco1 = false;
    deco2 = true;
    texture = "textures/grass-normal.bmp";
  }

let tree_grass_tile =
  {
    interact = NW;
    terrain = Grass;
    deco1 = true;
    deco2 = false;
    texture = "textures/grass-normal.bmp";
  }

let water_tile =
  {
    interact = INW Dialouge;
    terrain = Water;
    deco1 = false;
    deco2 = false;
    texture = "textures/water1.bmp";
  }

let top_right_grass =
  {
    interact = W;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/corner-grass-top-right.bmp";
  }

let top_left_grass =
  {
    interact = W;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/corner-grass-top-left.bmp";
  }

let bottom_right_grass =
  {
    interact = W;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/corner-grass-bottom-right.bmp";
  }

let bottom_left_grass =
  {
    interact = W;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/corner-grass-bottom-left.bmp";
  }

let path_tile =
  {
    interact = IW LoadMap;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/path-normal.bmp";
  }

let left_path_split =
  {
    interact = IW LoadMap;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/split-path-left.bmp";
  }

let right_path_split =
  {
    interact = IW LoadMap;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/split-path-right.bmp";
  }

let top_path_split =
  {
    interact = IW LoadMap;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/split-path-top.bmp";
  }

let bottom_path_split =
  {
    interact = IW LoadMap;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/split-path-bottom.bmp";
  }

let top_right_path =
  {
    interact = W;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/corner-path-top-right.bmp";
  }

let top_left_path =
  {
    interact = W;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/corner-path-top-left.bmp";
  }

let bottom_left_path =
  {
    interact = W;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/corner-path-bottom-left.bmp";
  }

let bottom_right_path =
  {
    interact = W;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/corner-path-bottom-right.bmp";
  }

let textured_path_tile =
  {
    interact = W;
    terrain = Path;
    deco1 = false;
    deco2 = false;
    texture = "textures/path-detail.bmp";
  }

let sand_tile =
  {
    interact = W;
    terrain = Sand;
    deco1 = false;
    deco2 = false;
    texture = "textures/path-detail.bmp";
  }

(*truncated nicknames*)
let tgt = tall_grass_tile
let gt = grass_tile
let gr = grass_tile_alt
let rgt = rock_grass_tile
let tr = tree_grass_tile
let trg = top_right_grass
let tlg = top_left_grass
let brg = bottom_right_grass
let blg = bottom_left_grass
let pa = path_tile
let pt = textured_path_tile
let rp = right_path_split
let lp = left_path_split
let tp = top_path_split
let bp = bottom_path_split
let wt = water_tile
let sd = sand_tile
let trp = top_right_path
let tlp = top_left_path
let blp = bottom_left_path
let brp = bottom_right_path

let tile_list =
  [
    tgt;
    gt;
    gr;
    rgt;
    tr;
    trg;
    brg;
    tlg;
    blg;
    pa;
    pt;
    tp;
    bp;
    rp;
    lp;
    trp;
    tlp;
    blp;
    brp;
    wt;
    sd;
  ]

(*maps - all should be 20*11 tiles, "smaller" or irregular sized maps can be
  wrapped with "non-space" tiles(to be implemented but basically no texture
  unwalkable tiles)

  DO NOT REMOVE THE OCAMLFORMAT DISABLE, UNLESS WE WANT TO UNNECESSARILY FULFILL
  THE LINE REQUIREMENT WITH JUST THIS FILE! AlTHOUGH WE MAY BREAK RECORD FOR
  3110 PROJECT LINES, IT WILL BECOME UNREADABLE(MORE THAN IT ALREADY IS) !*)
let shore1map =
  let tiles =
    [
      [tr; tr; tr; tr; tr; tr; tr; tr; rp; pa; lp; tr; tr; tr; tr; tr; tr; tr; tr; tr];
      [tr; tr; tr; tr; tr; tr; tr; gt; rp; pt; lp; gt; tr; tr; tr; tr; tr; tr; tr; tr];
      [gt; tr; tr; tr; gt; gt; brp; bp; tlg; pt; lp; gt; gt; gt; tr; tr; gt; gt; gt; gt];
      [bp; gt; gt; gt; gt; gr; rp; pa; pt; brg; tlp; gt; gt; gt; tr; gt; gt; gt; gt; gt];
      [pa; gt; gt; gt; gt; bp; tlg; brg; tp; tlp; gt; gt; gt; gt; gt; gt; gt; gt; tr; gt];
      [tp; gt; gt; gt; gt; tp; tp; tlp; gr; gr; gt; gt; gt; gt; gt; gt; gt; gt; gt; gt];
      [sd; sd; sd; sd; gt; gt; gt; gt; sd; sd; sd; sd; sd; sd; sd; sd; sd; sd; sd; sd];
      [wt; sd; sd; sd; sd; sd; sd; sd; sd; sd; sd; wt; wt; wt; wt; wt; wt; wt; wt; wt];
      [wt; wt; wt; wt; sd; sd; sd; sd; sd; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt];
      [wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt];
      [wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt];
    ]
  in
  let convert_row row = Array.of_list row in
  Array.of_list (List.map convert_row tiles) [@@ocamlformat "disable"]

let shore2map = 
  let tiles =
    [
      [tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr];
      [tr; tr; tr; tr; gr; gr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; gt; gt];
      [gt; tr; tr; gr; gt; gt; gt; gt; gt; gr; gr; gt; gt; gt; gr; gr; gt; gt; gt; gt];
      [gt; gt; gt; gt; gt; brp; bp; bp; bp; bp; bp; bp; bp; bp; bp; bp; bp; bp; bp; bp];
      [gt; gt; gt; gt; gt; rp; pa; pa; pa; pt; pa; pa; pa; pa; pa; pa; pa; pt; pt; pa];
      [gt; gt; gt; gt; gt; trp; tp; tp; blg; brg; tp; tp; tp; tp; tp; tp; tp; tp; tp; tp];
      [sd; sd; sd; sd; gt; gt; gt; gt; sd; sd; sd; sd; sd; sd; sd; sd; sd; sd; sd; sd];
      [wt; sd; sd; sd; sd; sd; sd; sd; sd; sd; sd; wt; wt; wt; wt; wt; wt; wt; wt; wt];
      [wt; wt; wt; wt; sd; sd; sd; sd; sd; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt];
      [wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt];
      [wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt];
    ]
  in
  let convert_row row = Array.of_list row in
  Array.of_list (List.map convert_row tiles) [@@ocamlformat "disable"]

let route4 = 
    let tiles =
      [
        [tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr];
        [tr; tr; tr; tr; gr; gr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; tr; gt; gt];
        [gt; tr; tr; gr; gt; gt; gt; gt; gt; gr; gr; gt; gt; gt; gr; gr; gt; gt; gt; gt];
        [gt; gt; gt; gt; gt; brp; bp; bp; bp; bp; bp; bp; bp; bp; bp; bp; bp; bp; bp; bp];
        [gt; gt; gt; gt; gt; rp; pa; pa; pa; pt; pa; pa; pa; pa; pa; pa; pa; pt; pt; pa];
        [gt; gt; gt; gt; gt; trp; tp; tp; blg; brg; tp; tp; tp; tp; tp; tp; tp; tp; tp; tp];
        [gt; tr; tr; gr; gt; tr; tr; gr; gr; gr; gr; gt; gt; gt; gr; gr; gt; gt; gt; gt];
        [gt; tr; tr; gr; gt; tr; gt; gr; gr; gr; gr; gt; gt; gt; gr; gr; gt; gt; gt; gt];
        [gt; tr; tr; tr; gr; tr; gt; gr; gr; gr; gr; gt; gt; gt; gr; gr; gt; gt; gt; gt];
        [gt; tr; tr; tr; tr; tr; gt; gr; gr; gr; gr; gt; gt; gt; gr; gr; gt; gt; gt; gt];
        [gt; tr; tr; tr; tr; tr; tr; tr; rp; pa; lp; tr; tr; tr; tr; tr; tr; tr; tr; tr];
      ]
    in
    let convert_row row = Array.of_list row in
    Array.of_list (List.map convert_row tiles) [@@ocamlformat "disable"]
