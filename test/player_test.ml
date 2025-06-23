(*Alcotest on cities graph*)
open Pandemicapi.City
open Pandemicapi.Card
open Pandemicapi.Player
open Pandemicapi.Board

let test_move_to_city_when_he_moves_flight () =
  Alcotest.(check bool)
    "Test if a player move to the city of a card when he moves by (direct) \
     flight"
    true
    (let map = create_map () in
     let cards = create_players_cards (CityMap.bindings map)
     and pTest = create_player "test" false map in
     let p', b' = player_add_card pTest (List.nth cards 0) in
     if not b' then false
     else
       let p', b' = player_move_by_direct_flight p' (List.nth cards 0) map in
       city_equals (get_current_city p')
         (get_city_of_card (List.nth cards 0) map)
       && b')

let test_move_to_city_when_he_charter_flight () =
  Alcotest.(check bool)
    "Test if a player move to the city when he moves by (charter) flight" true
    (let map = create_map () in
     let cardAtlanta = init_card "Atlanta" "blue" "Player"
     and pTest = create_player "test" false map in
     let p', b' = player_add_card pTest cardAtlanta in
     if not b' then false
     else
       let p', b' =
         player_move_by_charter_flight p' (get_city "Washington" map) map
       in
       city_equals (get_current_city p') (get_city "Washington" map) && b')

let test_player_contains_card_of_current_city () =
  Alcotest.(check bool)
    "Test if a player contains the card of his current_city" true
    (let map = create_map () in
     let cardAtlanta = init_card "Atlanta" "blue" "Player"
     and pTest = create_player "test" false map in
     let p', b' = player_add_card pTest cardAtlanta in
     if not b' then false else player_contains_card_of_current_city p' map)

let test_player_not_contains_card_of_current_city () =
  Alcotest.(check bool)
    "Test if a player not contains the card of his current_city" false
    (let map = create_map () in
     let cardWashington = init_card "Washington" "blue" "Player"
     and pTest = create_player "test" false map in
     let p', b' = player_add_card pTest cardWashington in
     if not b' then false else player_contains_card_of_current_city p' map)

let test_move_to_city_when_he_moves_car () =
  Alcotest.(check bool)
    "Test if a player move to the city next to him when he moves by (direct) \
     car"
    true
    (let map = create_map () in
     let pTest = create_player "test" false map in
     let p', b' = player_move_by_car pTest (get_city "Washington" map) in
     city_equals (get_current_city p') (get_city "Washington" map) && b')

let test_action_left_players () =
  Alcotest.(check int)
    "test if a player get -1 action when he did an action" 3
    (let map = create_map () in
     let cards = create_players_cards (CityMap.bindings map)
     and pTest = create_player "test" false map in
     let p', b' = player_add_card pTest (List.nth cards 0) in
     if not b' then -1
     else
       let p', _ = player_move_by_direct_flight p' (List.nth cards 0) map in
       get_actions_left p')

let test_action_left_0_players () =
  Alcotest.(check bool)
    "test if a player can not do a action with 0 action left" false
    (let map = create_map () in
     let cards = create_players_cards (CityMap.bindings map)
     and pTest = create_player "test" false map in
     let rec boucle i p b =
       if not b then b
       else
         let p', _ = player_add_card p (List.nth cards i) in
         let p', b' = player_move_by_direct_flight p' (List.nth cards 0) map in
         boucle (i + 1) p' b'
     in
     boucle 0 pTest true)

let test_discard_hand_player_when_he_moves_flight () =
  Alcotest.(check int)
    "Test if a player discard a card when he moves by (direct) fllight" 0
    (let map = create_map () in
     let cards = create_players_cards (CityMap.bindings map)
     and pTest = create_player "test" false map in
     let p', b' = player_add_card pTest (List.nth cards 0) in
     if not b' then -1
     else
       let p', _ = player_move_by_direct_flight p' (List.nth cards 0) map in
       List.length (get_hand p'))

let test_construct_research_station_without_the_card () =
  Alcotest.(check bool)
    "tess if a player can not construct a research station without the card"
    false
    (let map = create_map () in
     let pTest = create_player "test" false map in
     let pTest, _ = player_move_by_car pTest (get_city "Washington" map) in
     let _, b = construct_research_station pTest map in
     b)

let test_construct_research_station () =
  Alcotest.(check bool)
    "tess if a player can  construct a research station" true
    (let map = create_map () in
     let cardWashington = init_card "Washington" "blue" "Player"
     and pTest = create_player "test" false map in
     let pTest, b = player_add_card pTest cardWashington in
     if not b then false
     else
       let pTest, b = player_move_by_car pTest (get_city "Washington" map) in
       if not b then false
       else
         let pTest, b = construct_research_station pTest map in
         if not b then false else get_research_station (get_current_city pTest))

let test_move_by_shuttle () =
  Alcotest.(check bool)
    "tess if a player can move with shuttle" true
    (let map = create_map () in
     let pTest = create_player "test" false map in
     let cardJohannesburg = init_card "Johannesburg" "yellow" "Player"
     and cardAtlanta = init_card "Atlanta" "blue" "Player" in
     let pTest, _ = player_add_card pTest cardJohannesburg in
     let pTest, b = player_add_card pTest cardAtlanta in
     if not b then b
     else
       let pTest, b = player_move_by_direct_flight pTest cardJohannesburg map in
       if not b then b
       else
         let pTest, b = player_add_card pTest cardJohannesburg in
         if not b then b
         else
           let pTest, b = construct_research_station pTest map in
           if not false then b
           else
             let _, b =
               player_move_by_shuttle pTest (get_city "Atlanta" map) map
             in
             b)

let test_discover_remedy1 () =
  Alcotest.(check bool)
    "test if a player can discover a remedy" true
    (let map = create_map () in
     let pTest = create_player "test" false map in
     let infections_cards = create_infection_cards (CityMap.bindings map)
     and player_cards = create_players_cards (CityMap.bindings map) in
     let board = create_board map [ pTest ] infections_cards player_cards in
     let cardYellow1 = init_card "Johannesburg" "yellow" "Player"
     and a = init_card "Atlanta" "blue" "Player"
     and cardYellow2 = init_card "Los Angeles" "yellow" "Player"
     and cardYellow3 = init_card "Miami" "yellow" "Player"
     and cardYellow4 = init_card "Mexico City" "yellow" "Player"
     and cardYellow5 = init_card "Bogota" "yellow" "Player" in
     let pTest, _ = player_add_card pTest a in
     let pTest, _ = construct_research_station pTest map in
     let pTest, _ = player_add_card pTest cardYellow1 in
     let pTest, _ = player_add_card pTest cardYellow2 in
     let pTest, _ = player_add_card pTest cardYellow3 in
     let pTest, _ = player_add_card pTest cardYellow4 in
     let pTest, _ = player_add_card pTest cardYellow5 in
     let _, r, b = discover_remedy board pTest "yellow" in
     b && remed_is_discovered r "yellow")

let test_discover_remedy2 () =
  Alcotest.(check bool)
    "test if a player can discover a remedy" false
    (let map = create_map () in
     let pTest = create_player "test" false map in
     let infections_cards = create_infection_cards (CityMap.bindings map)
     and player_cards = create_players_cards (CityMap.bindings map) in
     let board = create_board map [ pTest ] infections_cards player_cards in
     let cardYellow1 = init_card "Johannesburg" "yellow" "Player"
     and a = init_card "Atlanta" "blue" "Player"
     and cardYellow2 = init_card "Los Angeles" "yellow" "Player"
     and cardYellow4 = init_card "Mexico City" "yellow" "Player"
     and cardYellow5 = init_card "Bogota" "yellow" "Player" in
     let pTest, _ = player_add_card pTest a in
     let pTest, _ = construct_research_station pTest map in
     let pTest, _ = player_add_card pTest cardYellow1 in
     let pTest, _ = player_add_card pTest cardYellow2 in
     let pTest, _ = player_add_card pTest cardYellow4 in
     let pTest, _ = player_add_card pTest cardYellow5 in
     let _, r, b = discover_remedy board pTest "yellow" in
     b && remed_is_discovered r "yellow")

let test_share_card1 () =
  Alcotest.(check bool)
    "test if a player can give a card to another player " true
    (let map = create_map () in
     let pTest1 = create_player "test" false map in
     let pTest2 = create_player "test2" false map in
     let card = init_card "Atlanta" "blue" "Player" in
     let pTest1, _ = player_add_card pTest1 card in
     let _, _, b, _ = p1_give_card_to_p2 pTest1 pTest2 map in
     b)

let test_share_card2 () =
  Alcotest.(check bool)
    "test if a player can take a card to another player " true
    (let map = create_map () in
     let pTest1 = create_player "test" false map in
     let pTest2 = create_player "test2" false map in
     let card = init_card "Atlanta" "blue" "Player" in
     let pTest2, _ = player_add_card pTest2 card in
     let _, _, b, _ = p1_take_card_to_p2 pTest1 pTest2 map in
     b)

let test_player_can_discover_remedy () =
  Alcotest.(check int)
    "test if a player who can't discover a remedy don't have the hand clear \
     after a check"
    6
    (let map = create_map () in
     let pTest = create_player "test" false map in
     let cardYellow1 = init_card "Johannesburg" "yellow" "Player"
     and a = init_card "Atlanta" "blue" "Player"
     and cardYellow2 = init_card "Paris" "blue" "Player"
     and cardYellow3 = init_card "Los Angeles" "yellow" "Player"
     and cardYellow4 = init_card "Mexico City" "yellow" "Player"
     and cardYellow5 = init_card "Washington" "blue" "Player" in
     let pTest, _ = player_add_card pTest a in
     let pTest, _ = player_add_card pTest cardYellow1 in
     let pTest, _ = player_add_card pTest cardYellow2 in
     let pTest, _ = player_add_card pTest cardYellow3 in
     let pTest, _ = player_add_card pTest cardYellow4 in
     let pTest, _ = player_add_card pTest cardYellow5 in
     let new_hand, _ = player_can_discover_remedy pTest false "yellow" in
     List.length new_hand)

(* let test_security_card () =
   Alcotest.(check bool)
     "test if a player can't take have a card not in the board" true
     (let map = create_map () in
      let pTest = create_player "test" false map in
      let cardYellow1 = init_card "SomeCardInexistant" "yellow" "Player" in
      try
        let _, _ = player_move_by_direct_flight pTest cardYellow1 map in
        false
      with Failure _ -> true) *)

(* let test_epidemic_card () =
   Alcotest.(check bool)
     "test if a player can take a card to another player " true
     (
     ) *)
let test_treat_disease () =
  Alcotest.(check bool)
    "test_if_treat_decrease_the_number_of_cubes" true
    (let map = create_map () in
     let new_map =
       CityMap.add "Atlanta" (set_cube (get_city "Atlanta" map) "blue" 3) map
     in
     let pTest = create_player "test" false new_map in
     let infections_cards = create_infection_cards (CityMap.bindings new_map)
     and player_cards = create_players_cards (CityMap.bindings new_map) in
     let board = create_board new_map [ pTest ] infections_cards player_cards in
     let player, _, possible = treat_disease_simplify board pTest "blue" in
     let city = get_current_city player in
     let cube = get_cube_colors city "blue" in
     cube = 2 && possible)

let test_treat_disease1 () =
  Alcotest.(check bool)
    "test if a player who want to treat a inexstant disease dont use a action"
    false
    (let map = create_map () in
     let pTest = create_player "test" false map in
     let infections_cards = create_infection_cards (CityMap.bindings map)
     and player_cards = create_players_cards (CityMap.bindings map) in
     let board = create_board map [ pTest ] infections_cards player_cards in
     let _, _, _, _, b = treat_disease board pTest "yellow" in
     b)

let () =
  let open Alcotest in
  run "Pandemicapi"
    [
      ( "Player",
        [
          test_case "test_discard_hand_player_when_he_moves_flight" `Quick
            test_discard_hand_player_when_he_moves_flight;
          test_case "test_move_to_city_when_he_moves_flight" `Quick
            test_move_to_city_when_he_moves_flight;
          test_case "test_action_left_players" `Quick test_action_left_players;
          test_case "test_action_left_0_players" `Quick
            test_action_left_0_players;
          test_case "test_move_to_city_when_he_moves_car" `Quick
            test_move_to_city_when_he_moves_car;
          test_case "test_move_to_city_when_he_charter_flight" `Quick
            test_move_to_city_when_he_charter_flight;
          test_case "test_player_contains_card_of_current_city" `Quick
            test_player_contains_card_of_current_city;
          test_case "test_player_not_contains_card_of_current_city" `Quick
            test_player_not_contains_card_of_current_city;
          test_case "test_construct_research_station_without_the_card" `Quick
            test_construct_research_station_without_the_card;
          test_case "test_construct_research_station" `Quick
            test_construct_research_station;
          test_case "test_move_by_shuttle" `Quick test_move_by_shuttle;
          test_case "test_discover_remedy1" `Quick test_discover_remedy1;
          test_case "test_discover_remedy2" `Quick test_discover_remedy2;
          test_case "test_share_card1" `Quick test_share_card1;
          test_case "test_share_card2" `Quick test_share_card2;
          test_case "test_treat_disease1" `Quick test_treat_disease1;
          test_case "test_player_can_discover_remedy" `Quick
            test_player_can_discover_remedy;
          test_case "test_treat_disease" `Quick test_treat_disease;
        ] );
    ]
