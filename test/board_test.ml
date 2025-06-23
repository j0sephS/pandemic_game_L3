(*Alcotest on cities graph*)
open Pandemicapi.City
open Pandemicapi.Card

let test_cities_number () =
  Alcotest.(check int)
    "check number of cities" 48
    (CityMap.cardinal (create_map ()))

let test_cities () =
  Alcotest.(check bool)
    "check if city exists" true
    (CityMap.mem "Ho Chi Minh City" (create_map ()))

let test_cities_neighbors () =
  Alcotest.(check int)
    "check number of neighbors of a neighbor" 5
    (let map = create_map () in
     List.length
       (get_neighbors
          (get_city
             (List.find
                (fun x -> x = "Shanghai")
                (get_neighbors (CityMap.find "Hong Kong" map)))
             map)))

let test_cards () =
  Alcotest.(check int)
    "check number of cards" 48
    (List.length (create_infection_cards (CityMap.bindings (create_map ()))))

let test_split_list () =
  Alcotest.(check int)
    "check number of cards don't change with split_list" 48
    (let list_of_length_of_list =
       List.map
         (fun x -> List.length x)
         (split_list (CityMap.bindings (create_map ())) 4)
     in
     Format.printf "%d" (List.hd (List.tl list_of_length_of_list));
     List.fold_left ( + ) 0 list_of_length_of_list)

let test_split_list_number () =
  Alcotest.(check int)
    "check number of split" 4
    (List.length (split_list (CityMap.bindings (create_map ())) 4))

let test_create_epidemic_and_event_cards () =
  Alcotest.(check int)
    "check number of cards"
    (48 + 5 + 4)
    (List.length
       (create_epidemic_and_event_cards
          (create_players_cards (CityMap.bindings (create_map ())))
          "Easy"))

let test_integrity_of_the_map () =
  Alcotest.(check bool)
    "check if the map is connected" true
    (let real_map = create_map () in
     let rec aux map visited =
       match map with
       | [] -> true
       | h :: t ->
           if List.mem h visited then aux t visited
           else aux (t @ get_neighbors (get_city h real_map)) (h :: visited)
     in
     aux [ "Atlanta" ] [])

let test_wrong_init_card () =
  Alcotest.check_raises "Expected Failure" (Failure "Invalid arguments")
    (fun () -> ignore (init_card "Albin" "RED" "Infection"))

let () =
  let open Alcotest in
  run "Pandemicapi"
    [
      ( "Cities",
        [
          test_case "test_graph_number_of_nodes" `Quick test_cities_number;
          test_case "test_graph" `Quick test_cities;
          test_case "test_graph_neighbors" `Quick test_cities_neighbors;
          test_case "test_integrity_of_the_map" `Quick test_integrity_of_the_map;
        ] );
      ("Board", [ test_case "test_board" `Quick test_cards ]);
      ( "Cards",
        [
          test_case "test_split_list" `Quick test_split_list;
          test_case "test_split_list_number" `Quick test_split_list_number;
          test_case "test_create_epidemic_and_event_cards" `Quick
            test_create_epidemic_and_event_cards;
          test_case "test_wrong_init_card" `Quick test_wrong_init_card;
        ] );
    ]
