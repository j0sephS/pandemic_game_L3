open Pandemicapi.City
open Pandemicapi.Card
open Pandemicapi.Player
open Pandemicapi.Board
open Pandemicapi.Game_api
open Pandemicapi.Action
open Pandemicapi.Bot

let test_epidemic () =
  Alcotest.(check bool)
    "test_epidemic" true
    (let map = create_map () in
     let player = create_player "test" false map in
     let player2 = create_player "Test2" false map in
     let board_start = start_game map ([ player ] @ [ player2 ]) "Easy" in
     let ce = init_card "" "" "Epidemic" in
     let board', _ = draw_epidemic board_start ce in
     match board' with
     | Running board_running ->
         CityMap.exists (fun _ c -> cube_3_or_more c) (get_cities board_running)
     | _ -> false)

let test_start_game_player_hand () =
  Alcotest.(check int)
    "test_player_hand" 4
    (let map = create_map () in
     let player = create_player "Test" false map in
     let player2 = create_player "Test2" false map in
     let board_start = start_game map ([ player ] @ [ player2 ]) "Easy" in
     match board_start with
     | Running board_running ->
         get_hand (List.hd (get_player_list board_running)) |> List.length
     | _ -> 0)

let test_start_game_infection () =
  Alcotest.(check int)
    "test_infection"
    (9 + 6 + 3)
    (let map = create_map () in
     let player = create_player "Test" false map in
     let player2 = create_player "Test2" false map in
     let board_start = start_game map ([ player ] @ [ player2 ]) "Easy" in
     match board_start with
     | Running board_running ->
         CityMap.fold
           (fun _ c acc -> acc + get_number_cube c)
           (get_cities board_running) 0
     | _ -> 0)

let test_propagation () =
  Alcotest.(check bool)
    "test_progagation_on_neigbors" true
    (let map = create_map () in
     let player = create_player "Test" false map in
     let player2 = create_player "Test2" false map in
     let board_start = start_game map ([ player ] @ [ player2 ]) "Easy" in
     match board_start with
     | Running board_running -> (
         let city_with_3 =
           CityMap.filter
             (fun _ c -> get_number_cube c = 3)
             (get_cities board_running)
         in
         let _, city = CityMap.choose city_with_3 in
         let new_board =
           add_infection_cube_to_city board_running city (get_city_color city)
         in
         match new_board with
         | Running new_board_running ->
             let a_city_neigbors =
               get_city
                 (List.hd (get_neighbors city))
                 (get_cities new_board_running)
             in
             let city_neigbors_cube = get_number_cube a_city_neigbors in
             city_neigbors_cube >= 1
         | _ -> false)
     | _ -> false)

let test_game_api_car () =
  Alcotest.(check bool)
    "test_game_api_car" true
    (let settings = create_settings 4 0 "Easy" in
     let game = new_game settings in
     let game = call_action game "Player 1" "move_by_car" "Chicago" in
     let game = call_action game "Player 1" "move_by_car" "Atlanta" in
     get_city_name (get_current_city (get_current_player game)) = "Atlanta")

let test_game_api_next_turn () =
  Alcotest.(check bool)
    "test_game_api_next_turn" true
    (let settings = create_settings 4 0 "Easy" in
     let game = new_game settings in
     let game = call_action game "Player 1" "move_by_car" "Chicago" in
     let game = call_action game "Player 1" "move_by_car" "Atlanta" in
     let game = call_action game "Player 1" "next_turn" "" in
     get_name (get_current_player game) = "Player 2")

let test_game_api_deal_cards () =
  Alcotest.(check int)
    "test_game_api_deal_cards" 2
    (let settings = create_settings 4 0 "Easy" in
     let game = new_game settings in
     List.length (get_hand (get_current_player game)))

let test_game_api_move_by_plane () =
  Alcotest.(check bool)
    "test_game_api_move_by_plane" true
    (let settings = create_settings 4 0 "Easy" in
     let game = new_game settings in
     let player_hand = get_hand (get_current_player game) in
     let c = List.hd player_hand in
     if get_type_card c = "Player" then
       let game =
         call_action game "Player 1" "move_by_direct_flight"
           (get_card_city_name (List.hd player_hand))
       in
       get_city_name (get_current_city (get_current_player game))
       = get_card_city_name (List.hd player_hand)
     else true)

let test_game_api_multiple_moves () =
  Alcotest.(check bool)
    "test_game_api_move_to_Madrid" true
    (let settings = create_settings 4 0 "Easy" in
     let game = new_game settings in
     let game = call_action game "Player 1" "move_by_car" "Washington" in
     let game = call_action game "Player 1" "move_by_car" "New York" in
     let game = call_action game "Player 1" "move_by_car" "Madrid" in
     get_city_name (get_current_city (get_current_player game)) = "Madrid")

let test_game_api_turn () =
  Alcotest.(check int)
    "test_game_api_reset_action_left_after_a_turn" 4
    (let settings = create_settings 2 0 "Easy" in
     let game = new_game settings in
     let game = call_action game "Player 1" "next_turn" "" in
     let game = call_action game "Player 2" "next_turn" "" in
     if get_nb_turns game = 1 then get_actions_left (get_current_player game)
     else 0)

let test_game_api_card_are_add () =
  Alcotest.(check bool)
    "test_if_a_turn_add_cards" true
    (let settings = create_settings 2 0 "Easy" in
     let game = new_game settings in
     let game = call_action game "Player 1" "next_turn" "" in
     let game = call_action game "Player 2" "next_turn" "" in
     List.length (get_hand (get_current_player game)) <= 6
     && List.length (get_hand (get_current_player game)) > 4
     (* can cause test failed if 2 epidemic are draw at the start but fix bug after*))

let test_game_api_max_card () =
  Alcotest.(check bool)
    "test_if_we_can_do_action_with_more_than_7_cards" true
    (let settings = create_settings 2 0 "Easy" in
     let game = new_game settings in
     let game = call_action game "Player 1" "next_turn" "" in
     let game = call_action game "Player 2" "next_turn" "" in
     let game = call_action game "Player 1" "next_turn" "" in
     let game = call_action game "Player 2" "next_turn" "" in
     try
       let game = call_action game "Player 1" "next_turn" "" in
       let game = call_action game "Player 2" "next_turn" "" in
       let _ = call_action game "Player 1" "next_turn" "" in
       false
     with Failure _ -> true)

let test_game_api_bot () =
  Alcotest.(check bool)
    "test_if_bot_play" true
    (let settings = create_settings 0 2 "Easy" in
     let game = new_game settings in
     let player_bot = get_current_player game in
     let game = call_action_bot game bot_make_action in
     get_actions_left (get_current_player game) = 3
     || get_player_id player_bot <> get_player_id (get_current_player game))

let test_game_api_bot_full () =
  Alcotest.(check bool)
    "test_if_bot_dont_turn_infinite" false
    (let settings = create_settings 0 2 "Easy" in
     let game = new_game settings in
     let rec aux game =
       let game = call_action_bot game bot_make_action in
       if get_status game = true then aux game else false
     in
     aux game)

let test_game_api_move_plane () =
  Alcotest.(check bool)
    "test_if_bot_play" true
    (let settings = create_settings 0 2 "Easy" in
     let game = new_game settings in
     let player_bot = get_current_player game in
     let player_hand = get_hand player_bot in
     let c = List.hd player_hand in
     let game =
       call_action game "Player (bot) 1" "move_by_direct_flight"
         (get_card_city_name c)
     in
     get_city_name (get_current_city (get_current_player game))
     = get_card_city_name c)

let test_game_next_turn () =
  Alcotest.(check bool)
    "test_if_bot_play" true
    (let settings = create_settings 0 2 "Easy" in
     let game = new_game settings in
     let player_bot = get_current_player game in
     let game = call_action game "Player (bot) 1" "next_turn" "" in
     get_name (get_current_player game) <> get_name player_bot)

let test_game_a_fail_action () =
  Alcotest.(check bool)
    "a fail action don't change the action left" true
    (let settings = create_settings 2 0 "Easy" in
     let game = new_game settings in
     let game = call_action game "Player 1" "move_by_car" "Chicago" in
     let game = call_action game "Player 1" "move_by_car" "Atlanta" in
     let game = call_action game "Player 1" "move_by_car" "Atlanta" in
     get_actions_left (get_current_player game) = 2)

let test_game_a_fail_action_event () =
  Alcotest.(check bool)
    "a fail event action don't change the action left" true
    (let settings = create_settings 2 0 "Easy" in
     let game = new_game settings in
     let game = call_action game "Player 1" "government_grant" "Chicago" in
     get_actions_left (get_current_player game) = 4)

let test_bot_good_draw () =
  Alcotest.(check int)
    "test if bot good draw don't discard too much" 7
    (let card_player_list =
       create_players_cards (CityMap.bindings (create_map ()))
     in
     Random.self_init ();
     let card_player_list =
       List.sort (fun _ _ -> Random.int 2 - 1) card_player_list
     in
     let take_9_card =
       let rec aux acc card_list = function
         | [] -> card_list
         | h :: t ->
             if acc = 9 then card_list else aux (acc + 1) (h :: card_list) t
       in
       aux 0 [] card_player_list
     in
     List.length (good_draw take_9_card))

let test_start_game_player_hand_3 () =
  Alcotest.(check bool)
    "test_player_hand" true
    (let map = create_map () in
     let player = create_player "Test" false map in
     let player2 = create_player "Test2" false map in
     let player3 = create_player "Test3" false map in
     let board_start =
       start_game map ([ player ] @ [ player2 ] @ [ player3 ]) "Easy"
     in
     match board_start with
     | Running board_running -> (
         match get_player_list board_running with
         | [] -> false
         | l -> List.for_all (fun p -> List.length (get_hand p) = 3) l)
     | _ -> false)

let test_start_game_player_hand_4 () =
  Alcotest.(check bool)
    "test_player_hand" true
    (let map = create_map () in
     let player = create_player "Test" false map in
     let player2 = create_player "Test2" false map in
     let player3 = create_player "Test3" false map in
     let player4 = create_player "Test4" false map in
     let board_start =
       start_game map
         ([ player ] @ [ player2 ] @ [ player3 ] @ [ player4 ])
         "Easy"
     in
     match board_start with
     | Running board_running -> (
         match get_player_list board_running with
         | [] -> false
         | l -> List.for_all (fun p -> List.length (get_hand p) = 2) l)
     | _ -> false)

let test_game_difficulty_easy () =
  Alcotest.(check bool)
    "test_if_game_works_with_difficulty_easy_and_bot" false
    (let settings = create_settings 0 2 "Easy" in
     let game = new_game settings in
     let rec aux game =
       let game = call_action_bot game bot_make_action in
       if get_status game = true then aux game else false
     in
     aux game)

let test_game_difficulty_standard () =
  Alcotest.(check bool)
    "test_if_game_works_with_difficulty_standard_and_bot" false
    (let settings = create_settings 0 2 "Standard" in
     let game = new_game settings in
     let rec aux game =
       let game = call_action_bot game bot_make_action in
       if get_status game = true then aux game else false
     in
     aux game)

let test_game_difficulty_heroic () =
  Alcotest.(check bool)
    "test_if_game_works_with_difficulty_heroic_and_bot" false
    (let settings = create_settings 0 2 "Heroic" in
     let game = new_game settings in
     let rec aux game =
       let game = call_action_bot game bot_make_action in
       if get_status game = true then aux game else false
     in
     aux game)

let test_game_atlanta_research () =
  Alcotest.(check bool)
    "test_game_atlanta_research" true
    (let settings = create_settings 0 2 "Easy" in
     let game = new_game settings in
     match get_game_state game with
     | Running board ->
         get_research_station (get_city "Atlanta" (get_cities board))
     | _ -> false)

let () =
  let open Alcotest in
  run "Pandemicapi"
    [
      ( "Game_api_actions",
        [
          test_case "test_move_car" `Quick test_game_api_car;
          test_case "test_next_turn" `Quick test_game_api_next_turn;
          test_case "test_deals_cards" `Quick test_game_api_deal_cards;
          test_case "test_move_plane" `Quick test_game_api_move_by_plane;
          test_case "test_multiple_moves" `Quick test_game_api_multiple_moves;
          test_case "test_api_turn" `Quick test_game_api_turn;
          test_case "test_card_add" `Quick test_game_api_card_are_add;
          test_case "test_max_card" `Quick test_game_api_max_card;
          test_case "test_next_turn" `Quick test_game_next_turn;
          test_case "test_move_plane" `Quick test_game_api_move_plane;
          test_case "test_fail_action" `Quick test_game_a_fail_action;
          test_case "test_fail_action_event" `Quick
            test_game_a_fail_action_event;
        ] );
      ( "Game_api",
        [
          test_case "test_epidemic" `Quick test_epidemic;
          test_case "test_start_game" `Quick test_start_game_player_hand;
          test_case "test_start_game_3" `Quick test_start_game_player_hand_3;
          test_case "test_start_game_4" `Quick test_start_game_player_hand_4;
          test_case "test_infection" `Quick test_start_game_infection;
          test_case "test_propagation" `Quick test_propagation;
        ] );
      ( "Bot",
        [
          test_case "test_bot" `Quick test_game_api_bot;
          test_case "test_bot_full" `Quick test_game_api_bot_full;
          test_case "test_bot_good_draw" `Quick test_bot_good_draw;
        ] );
      ( "Test_game",
        [
          test_case "test_easy" `Quick test_game_difficulty_easy;
          test_case "test_standard" `Quick test_game_difficulty_standard;
          test_case "test_heroic" `Quick test_game_difficulty_heroic;
          test_case "test_game_atlanta_research" `Quick
            test_game_atlanta_research;
        ] );
    ]
