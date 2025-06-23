open City
open Board
open Player
open Game_api

let make_a_action board (new_player, havemove) =
  if havemove then
    let new_board = player_update board new_player in
    new_board
  else board

let action_move_by_car board player city =
  match board with
  | Running b ->
      make_a_action board
        (player_move_by_car player (get_city city (get_cities b)))
  | s -> s

let action_move_by_charter_flight board player city =
  match board with
  | Running b ->
      let city_map = get_cities b in
      make_a_action board
        (player_move_by_charter_flight player (get_city city city_map) city_map)
  | s -> s

let action_move_by_direct_flight board player city =
  match board with
  | Running b ->
      let city_map = get_cities b in
      (* make_a_action board player city player_move_by_direct_flight(player (get_city city city_map) city_map)
         Il faut mettre un argument de direct_flight en city et pas en card *)
      make_a_action board
        (player_move_by_direct_flight player
           (get_card_of_city_name city player)
           city_map)
  | s -> s
(*en attendant*)

let action_move_by_shuttle board player city =
  match board with
  | Running b ->
      let city_map = get_cities b in
      make_a_action board
        (player_move_by_shuttle player (get_city city city_map) city_map)
  | s -> s

let action_construct_research_station board player =
  match board with
  | Running b ->
      make_a_action board (construct_research_station player (get_cities b))
  | s -> s

let action_treat_disease board player color =
  if test_string_color color = false then failwith "Error, this is not a color"
  else
    match board with
    | Running b ->
        let new_player, new_board, havemove =
          treat_disease_simplify b player color
        in
        make_a_action (Running new_board) (new_player, havemove)
    | s -> s

let action_discover_remedy board player color =
  match board with
  | Running b ->
      if test_string_color color = false then
        failwith "Error, this is not a color"
      else
        let new_player, new_board, havemove =
          discover_remedy_simplify b player color
        in
        make_a_action new_board (new_player, havemove)
  | s -> s

let pass_turn board player = make_a_action board (player_pass_turn player, true)

let action_discard board player (card : string) =
  make_a_action board (player_discard player card)

let action_government_grant board player city =
  match board with
  | Running b -> (
      try Running (government_grant player city b)
      with _ ->
        Format.printf
          "You can't use this card@ or this city is not in the board@.";
        board)
  | s -> s

let action_airlift board player target city =
  match board with
  | Running b -> (
      (*do airlift but target another user*)
      try Running (airlift player target city b) (*the second player*)
      with _ ->
        Format.printf
          "You can't use this card@ or this city is not in the board@.";
        board)
  | s -> s

let action_one_quiet_night board player =
  match board with
  | Running b -> (
      try Running (one_quiet_night b player)
      with _ ->
        Format.printf "You can't use this card@.";
        board)
  | s -> s

let action_forecast board player =
  match board with
  | Running b -> (
      try Running (forecast player b)
      with _ ->
        Format.printf "You can't use this card@.";
        board)
  | s -> s

let action_resilient_population board player card =
  match board with
  | Running b -> (
      try Running (resilient_population_not_interactive b player card)
      with _ ->
        Format.printf
          "You can't use this card or this card is not in the infection \
           discard@.";
        board)
  | s -> s

let find_player_in_player_list player_name game_state =
  match game_state with
  | Running b -> (
      let player_list = get_player_list b in
      try List.find (fun x -> get_name x = player_name) player_list
      with _ ->
        failwith
          ("Error, (" ^ player_name
         ^ ") is not a player in this game, try again."))
  | _ -> failwith "Error, game is not running"

let call_action (game_api : game_api) (player : string) (action_type : string)
    (target : string) : game_api =
  if
    get_player_id (find_player_in_player_list player (get_game_state game_api))
    <> get_player_id (get_current_player game_api)
  then
    failwith
      ("Error, this is not (" ^ player ^ ")'s turn, try to end ("
      ^ get_name (get_current_player game_api)
      ^ ")'s turn before.")
  else
    let new_board =
      let game_state = get_game_state game_api in
      match action_type with
      | "government_grant" ->
          Some
            (action_government_grant game_state
               (get_current_player game_api)
               target)
      | "airlift" ->
          Some
            (action_airlift (*do airlift but target another user*)
               game_state
               (get_current_player game_api)
               (get_current_player game_api)
               target)
      | "one_quiet_night" ->
          Some (action_one_quiet_night game_state (get_current_player game_api))
      | "forecast" ->
          Some (action_forecast game_state (get_current_player game_api))
      | "resilient_population" ->
          Some
            (action_resilient_population game_state
               (get_current_player game_api)
               target)
      | "discard" ->
          Some (action_discard game_state (get_current_player game_api) target)
      | _ -> (
          if List.length (get_hand (get_current_player game_api)) > 7 then (
            match action_type with
            | "discard" ->
                Some
                  (action_discard game_state
                     (get_current_player game_api)
                     target)
            | _ ->
                Format.printf
                  "You have too many cards. You must discard one before \
                   playing@.";
                None)
          else
            match action_type with
            | "move_by_car" ->
                Some
                  (action_move_by_car game_state
                     (get_current_player game_api)
                     target)
            | "move_by_direct_flight" ->
                Some
                  (action_move_by_direct_flight game_state
                     (get_current_player game_api)
                     target)
            | "move_by_charter_flight" ->
                Some
                  (action_move_by_charter_flight game_state
                     (get_current_player game_api)
                     target)
            | "move_by_shuttle" ->
                Some
                  (action_move_by_shuttle game_state
                     (get_current_player game_api)
                     target)
            | "next_turn" ->
                Some (pass_turn game_state (get_current_player game_api))
            | "construct" ->
                Some
                  (action_construct_research_station game_state
                     (get_current_player game_api))
            | "treat" ->
                Some
                  (action_treat_disease game_state
                     (get_current_player game_api)
                     target)
            | "discover_remedy" ->
                Some
                  (action_discover_remedy game_state
                     (get_current_player game_api)
                     target)
            (*do share_knowledge*)
            | _ -> None)
    in
    if new_board = None then
      failwith ("Error, (" ^ action_type ^ ") is not an action")
    else update_game game_api (Option.get new_board)

let call_action_bot game_api bot_make_action =
  let current_player = get_current_player game_api in
  let board = get_game_state game_api in
  update_game game_api (bot_make_action board current_player)

let show_city_info (game_api : game_api) (city : string) : unit =
  match get_game_state game_api with
  | Running b ->
      let cities = get_cities b in
      Format.printf "%a@." ppcity_full (get_city city cities, cities)
  | Lose s -> Format.printf "%s@." s
  | Won -> Format.printf "You Win"
