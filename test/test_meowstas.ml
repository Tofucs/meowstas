open OUnit2
open Meowstas.Tile
open Meowstas.Map

(*testing creation of maps*)

let test_tiles1 =
  [| [| W; NW; NW; NW |]; [| W; W; NW; NW |]; [| W; NW; NW; W |] |]

let test_tiles2 = [| [| W |]; [| NW |]; [| W |]; [| NW |] |]
let test_tiles3 = [| [| W; W; NW |]; [| W; W; NW |]; [| W; W; NW |] |]
let test_player = Meowstas.Player.make ()
let test_map1 = make () "test map 1" test_tiles1 (3, 4)
let test_map2 = make () "test map 2" test_tiles2 (4, 1)
let test_map3 = make () "test map 3" test_tiles3 (3, 3)

let tests =
  "test suite"
  >::: [
         ( "empty map has size 0" >:: fun _ ->
           assert_equal (size (make () "empty" [||] (0, 0))) (0, 0) );
         ( "name gets map name" >:: fun _ ->
           assert_equal (name test_map1) "test map 1" );
         ( "name gets map name" >:: fun _ ->
           assert_equal (name test_map2) "test map 2" );
         ( "name gets map name" >:: fun _ ->
           assert_equal (name test_map3) "test map 3" );
         ("size gets map size" >:: fun _ -> assert_equal (size test_map1) (3, 4));
         ("size gets map size" >:: fun _ -> assert_equal (size test_map2) (4, 1));
         ("size gets map size" >:: fun _ -> assert_equal (size test_map3) (3, 3));
         ( "query_tile gets tile at position test 1" >:: fun _ ->
           assert_equal (query_tile test_map1 (Some 0, Some 0)) W );
         ( "query_tile gets tile at position test 2" >:: fun _ ->
           assert_equal (query_tile test_map1 (Some 0, Some 1)) NW );
         ( "query_tile gets tile at position test 3" >:: fun _ ->
           assert_equal (query_tile test_map1 (Some 1, Some 0)) IW );
         ( "query_tile gets tile at position test 4" >:: fun _ ->
           assert_equal (query_tile test_map1 (Some 1, Some 3)) IW );
         ( "query_tile gets tile at position test 5" >:: fun _ ->
           assert_equal (query_tile test_map1 (Some 2, Some 1)) NW );
         ( "query_tile gets tile at position test 6" >:: fun _ ->
           assert_equal (query_tile test_map2 (Some 0, Some 0)) W );
         ( "query_tile gets tile at position test 7" >:: fun _ ->
           assert_equal (query_tile test_map2 (Some 1, Some 0)) NW );
         ( "query_tile gets tile at position test 8" >:: fun _ ->
           assert_equal (query_tile test_map2 (Some 2, Some 0)) IW );
         ( "query_tile gets tile at position test 9" >:: fun _ ->
           assert_equal (query_tile test_map2 (Some 3, Some 0)) INW );
         ( "query_tile gets tile at position test 10" >:: fun _ ->
           assert_equal (query_tile test_map3 (Some 0, Some 0)) W );
         ( "query_tile gets tile at position test 11" >:: fun _ ->
           assert_equal (query_tile test_map3 (Some 2, Some 2)) IW );
         ( "query_tile gets tile at position test 12" >:: fun _ ->
           assert_equal (query_tile test_map3 (Some 1, Some 2)) IW );
         ( "query_tile raises PlayerUninstantiated when given None test 1"
         >:: fun _ ->
           assert_raises PlayerUninstantiated (fun _ ->
               query_tile test_map1 (None, None)) );
         ( "query_tile raises PlayerUninstantiated when given None test 2"
         >:: fun _ ->
           assert_raises PlayerUninstantiated (fun _ ->
               query_tile test_map2 (None, None)) );
         ( "instantiated player has velocity Idle" >:: fun _ ->
           assert_equal test_player.velocity Meowstas.Player.Idle );
         ( "instantiated player has state South" >:: fun _ ->
           assert_equal test_player.state Meowstas.Player.South );
         ( "instantiated player has same position test 1" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 2, Some 0);
              get_player_pos test_map1)
             (Some 2, Some 0) );
         ( "instantiated player has same position test 2" >:: fun _ ->
           assert_equal
             (create_player test_map2 (Some 0, Some 0);
              get_player_pos test_map2)
             (Some 0, Some 0) );
         ( "idle move does not change position test 1" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 2, Some 0);
              update_location test_map1 Idle;
              get_player_pos test_map1)
             (Some 2, Some 0) );
         ( "idle move does not change position test 2" >:: fun _ ->
           assert_equal
             (create_player test_map2 (Some 0, Some 0);
              update_location test_map2 Idle;
              get_player_pos test_map2)
             (Some 0, Some 0) );
         ( "up move test 1" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 2, Some 0);
              update_location test_map1 Up;
              get_player_pos test_map1)
             (Some 2, Some 0) );
         ( "up move test 2" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 1, Some 0);
              update_location test_map1 Up;
              get_player_pos test_map1)
             (Some 1, Some 1) );
         ( "up move test 3" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 1, Some 0);
              update_location test_map1 Up;
              update_location test_map1 Up;
              get_player_pos test_map1)
             (Some 1, Some 1) );
         ( "left move test 1" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 1, Some 1);
              update_location test_map1 Left;
              get_player_pos test_map1)
             (Some 1, Some 1) );
         ( "left move test 2" >:: fun _ ->
           assert_equal
             (create_player test_map2 (Some 1, Some 0);
              update_location test_map2 Left;
              get_player_pos test_map2)
             (Some 0, Some 0) );
         ( "left move test 3" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 2, Some 0);
              update_location test_map1 Left;
              get_player_pos test_map1)
             (Some 1, Some 0) );
         ( "left move test 4" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 2, Some 0);
              update_location test_map1 Left;
              update_location test_map1 Left;
              get_player_pos test_map1)
             (Some 0, Some 0) );
         ( "right move test 1" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 0, Some 0);
              update_location test_map1 Right;
              get_player_pos test_map1)
             (Some 1, Some 0) );
         ( "right move test 2" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 0, Some 0);
              update_location test_map1 Right;
              update_location test_map1 Right;
              get_player_pos test_map1)
             (Some 2, Some 0) );
         ( "right move test 3" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 1, Some 1);
              update_location test_map1 Right;
              get_player_pos test_map1)
             (Some 1, Some 1) );
         ( "right move test 4" >:: fun _ ->
           assert_equal
             (create_player test_map2 (Some 2, Some 0);
              update_location test_map2 Right;
              get_player_pos test_map2)
             (Some 2, Some 0) );
         ( "down move test 1" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 0, Some 2);
              update_location test_map1 Down;
              get_player_pos test_map1)
             (Some 0, Some 2) );
         ( "down move test 2" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 1, Some 1);
              update_location test_map1 Down;
              get_player_pos test_map1)
             (Some 1, Some 0) );
         ( "multi move test 1" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 2, Some 0);
              update_location test_map1 Left;
              update_location test_map1 Up;
              get_player_pos test_map1)
             (Some 1, Some 1) );
         ( "multi move test 2" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 2, Some 0);
              update_location test_map1 Left;
              update_location test_map1 Up;
              update_location test_map1 Right;
              get_player_pos test_map1)
             (Some 1, Some 1) );
         ( "multi move test 3" >:: fun _ ->
           assert_equal
             (create_player test_map1 (Some 2, Some 0);
              update_location test_map1 Left;
              update_location test_map1 Up;
              update_location test_map1 Right;
              update_location test_map1 Down;
              update_location test_map1 Left;
              get_player_pos test_map1)
             (Some 0, Some 0) );
         ( "multi move test 4" >:: fun _ ->
           assert_equal
             (create_player test_map3 (Some 1, Some 1);
              update_location test_map3 Right;
              update_location test_map3 Up;
              update_location test_map3 Left;
              update_location test_map3 Down;
              get_player_pos test_map3)
             (Some 1, Some 1) );
         ( "multi move test 5" >:: fun _ ->
           assert_equal
             (create_player test_map3 (Some 0, Some 0);
              update_location test_map3 Up;
              update_location test_map3 Up;
              update_location test_map3 Right;
              update_location test_map3 Down;
              update_location test_map3 Down;
              get_player_pos test_map3)
             (Some 1, Some 0) );
         ( "multi move test 6" >:: fun _ ->
           assert_equal
             (create_player test_map3 (Some 0, Some 0);
              update_location test_map3 Right;
              update_location test_map3 Up;
              update_location test_map3 Right;
              update_location test_map3 Up;
              get_player_pos test_map3)
             (Some 2, Some 2) );
         ( "multi move test 7" >:: fun _ ->
           assert_equal
             (create_player test_map3 (Some 2, Some 2);
              update_location test_map3 Down;
              update_location test_map3 Left;
              update_location test_map3 Down;
              update_location test_map3 Left;
              get_player_pos test_map3)
             (Some 0, Some 0) );
         ( "multi move test 8" >:: fun _ ->
           assert_equal
             (create_player test_map3 (Some 2, Some 0);
              update_location test_map3 Left;
              update_location test_map3 Up;
              update_location test_map3 Left;
              update_location test_map3 Up;
              get_player_pos test_map3)
             (Some 0, Some 2) );
         ( "multi move test 9" >:: fun _ ->
           assert_equal
             (create_player test_map3 (Some 0, Some 2);
              update_location test_map3 Down;
              update_location test_map3 Right;
              update_location test_map3 Down;
              update_location test_map3 Right;
              get_player_pos test_map3)
             (Some 2, Some 0) );
         ( "multi move test 10" >:: fun _ ->
           assert_equal
             (create_player test_map3 (Some 1, Some 1);
              update_location test_map3 Right;
              update_location test_map3 Down;
              update_location test_map3 Left;
              update_location test_map3 Left;
              update_location test_map3 Up;
              update_location test_map3 Up;
              update_location test_map3 Right;
              update_location test_map3 Right;
              update_location test_map3 Down;
              get_player_pos test_map3)
             (Some 2, Some 1) );
       ]

let _ = run_test_tt_main tests
