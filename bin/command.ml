open Pandemicapi.Board
open Pandemicapi.Player
open Pandemicapi.City
open Pandemicapi.Action

let unable_to_do_action () =
  Format.printf "Error, unable to do this action@ ";
  None

let catch_city_error city_name cities =
  try Some (get_city city_name cities) with Failure _ -> None

let parse_move command board player =
  match board with
  | Running board_running -> (
      let cities = get_cities board_running in
      match command with
      | "car" ->
          Format.printf "Here are the cities you can chose to go to:@ %a%!"
            ppneighbors
            (get_current_city player, cities);
          Format.printf "move > car > %!";
          let city = catch_city_error (read_line ()) cities in
          if city = None then (
            Format.printf "Invalid city name@.";
            None)
          else
            Some
              (action_move_by_car board player
                 (get_city_name (Option.get city)))
      | "plane" ->
          Format.printf
            "Use a card to move to the corresponding city,@ you can also use \
             the card of your current city to move anywhere.@.";
          Format.printf "move > plane > %!";
          let city = catch_city_error (read_line ()) cities in
          if city = None then unable_to_do_action ()
          else if city_equals (Option.get city) (get_current_city player) then
            let () =
              Format.printf
                "Chose a city to move to between all the cities on the map.@.";
              Format.printf "move > plane > %s > %!"
                (get_city_name (Option.get city))
            in
            let city' = catch_city_error (read_line ()) cities in
            if city' = None then unable_to_do_action ()
            else
              Some
                (action_move_by_charter_flight board player
                   (get_city_name (Option.get city')))
          else
            Some
              (action_move_by_direct_flight board player
                 (get_city_name (Option.get city)))
      | "shuttle" ->
          Format.printf "Cities with a research station:@ %a%!"
            ppresearch_stations
            (get_current_city player, cities);
          Format.printf "move > shuttle > %!";
          let city = catch_city_error (read_line ()) cities in
          if city = None then unable_to_do_action ()
          else
            Some
              (action_move_by_shuttle board player
                 (get_city_name (Option.get city)))
      | _ -> None)
  | _ -> failwith "parse_move: board is not running"

let parse_status command board player =
  match board with
  | Running board_running -> (
      match command with
      | "player" ->
          Format.printf "%a@." ppplayer player;
          None
      | "city" ->
          Format.printf "%a@." ppcity_full
            (get_current_city player, get_cities board_running);
          None
      | "world_city" -> (
          Format.printf "Enter the name of the city you want to check:@.";
          try
            Format.printf "%a@." ppcity_full
              ( get_city (read_line ()) (get_cities board_running),
                get_cities board_running );
            None
          with Failure _ ->
            Format.printf "Invalid city name@.";
            None)
      | "all_city" ->
          CityMap.iter
            (fun _ c ->
              Format.printf "%a@." ppcity_full (c, get_cities board_running))
            (get_cities board_running);
          None
      | _ -> None)
  | _ -> failwith "parse_status: board is not running"

let update_player_with_board board player =
  match board with
  | Running b ->
      List.find (fun p -> get_name p = get_name player) (get_player_list b)
  | _ -> failwith "update_player_with_board: board is not running"

let rec parse_event command board player =
  let airlift_validation player city_name =
    Format.printf "%s do you want to be moved to %s @? (Yes/No)@?"
      (get_name player) city_name;
    let respond = read_line () in
    if respond = "No" then false else true
  in

  match command with
  | "help" ->
      Format.printf
        "Commands: help, airlift, forecast, government_grant, one_quiet_night, \
         resilient_population@.";
      Format.printf "event > %!";
      parse_event (read_line ()) board player
  | "one_quiet_night" -> (
      try Some (action_one_quiet_night board player)
      with Failure _ ->
        Format.printf "You can't use this card@.";
        None)
  | "government_grant" -> (
      Format.printf
        "Enter the name of the city you want to add a research station to:@.";
      Format.printf "event > government_grant > %!";
      try Some (action_government_grant board player (read_line ()))
      with Failure _ ->
        Format.printf "You can't use this card@.";
        None)
  | "airlift" -> (
      Format.printf "Enter the name of the player you want to move:@.";
      let player_name = read_line () in
      match board with
      | Running b ->
          if List.exists (fun x -> get_name x = player_name) (get_player_list b)
          then (
            Format.printf "Enter the name of the city you want to move to:@.";
            Format.printf "event > airlift > %!";
            let city_name = read_line () in
            let player_to_move =
              List.find (fun x -> get_name x = player_name) (get_player_list b)
            in
            if not (get_player_id player_to_move = get_player_id player) then (
              if not (airlift_validation player_to_move city_name) then
                Format.printf "%s don't want to move.@" (get_name player);
              None)
            else
              try Some (action_airlift board player player_to_move city_name)
              with Failure _ ->
                Format.printf
                  "You can't use this card or this city doesn't exist@.";
                None)
          else (
            Format.printf "Invalid player name@.";
            None)
      | _ -> failwith "parse_event: board is not running")
  | "forecast" -> (
      try Some (forecast_interactive player board)
      with Failure _ ->
        Format.printf "You can't use this card@.";
        None)
  | "resilient_population" -> (
      try Some (resilient_population board player)
      with Failure _ ->
        Format.printf "You can't use this card or this city doesn't exist@.";
        None)
  | _ ->
      Format.printf "Invalid command@.";
      None

let parse command board player =
  if List.length (get_hand player) > 7 then (
    match command with
    | "discard" ->
        Format.printf "Enter the name of the card you want to discard:@.";
        let target = read_line () in
        let p, havediscard = player_discard player target in
        if havediscard then Some (make_a_action board (p, havediscard))
        else (
          Format.printf "You don't have this card@.";
          None)
    | "show_hand" ->
        Format.printf "%a@." pphand player;
        None
    | "help" ->
        Format.printf "Commands: help, discard, show_hand, status, event@.";
        None
    | "status" ->
        Format.printf
          "You can check the status of: player, city, world_city, all_city@ ";
        Format.printf "status > %!";
        parse_status (read_line ()) board player
    | "event" ->
        Format.printf "Enter the name of the event card you want to use:@.";
        Format.printf "event > %!";
        parse_event (read_line ()) board player
    | _ ->
        Format.printf
          "You have too many cards. You must discard one before playing@.";
        None)
  else
    match command with
    | "help" ->
        Format.printf
          "Commands: help, move, construct, discover_remedy, treat, status, \
           next_turn, quit, event@.";
        None
    | "quit" ->
        Format.printf "Are you sure you want to quit? (y/n)@.";
        if read_line () = "y" then exit 0 else None
    | "event" ->
        Format.printf
          "Enter the name of the event card you want to use (help):@.";
        Format.printf "event > %!";
        parse_event (read_line ()) board player
    | "move" ->
        Format.printf "You can move by: car, plane, shuttle@ ";
        Format.printf "move > %!";
        parse_move (read_line ()) board player
    | "status" ->
        Format.printf
          "You can check the status of: player, city, world_city, all_city@ ";
        Format.printf "status > %!";
        parse_status (read_line ()) board player
    | "construct" -> (
        match board with
        | Running b ->
            let p, b = construct_research_station player (get_cities b) in
            if b then Some (make_a_action board (p, b))
            else (
              Format.printf "You can't construct a research station here@.";
              None)
        | _ -> failwith "parse: board is not running")
    | "treat" ->
        let color =
          Format.printf "Enter the color of the disease you want to treat:@.";
          read_line ()
        in
        if test_string_color color then
          match board with
          | Running b ->
              let p, board, b = treat_disease_simplify b player color in
              if b then Some (make_a_action (Running board) (p, b))
              else (
                Format.printf "You can't treat this city@.";
                None)
          | _ -> failwith "parse: board is not running"
        else (
          Format.printf "Invalid color@.";
          None)
    | "discover_remedy" ->
        let color =
          Format.printf "Enter the color of the remedy you want to discover:@.";
          read_line ()
        in
        if test_string_color color then
          match board with
          | Running board_running ->
              let p, board, b =
                discover_remedy_simplify board_running player color
              in
              if b then Some (make_a_action board (p, b))
              else (
                Format.printf "You can't discover a remedy for %s@." color;
                None)
          | _ -> failwith "parse: board is not running"
        else (
          Format.printf "Invalid color@.";
          None)
    | "next_turn" ->
        let p = player_pass_turn player in
        Some (make_a_action board (p, true))
    | _ ->
        Format.printf "Invalid command@.%!";
        None

let command_line_display board player =
  let rec loop () =
    Format.printf "> %!";
    let command = read_line () in
    match parse command board player with
    | None -> loop ()
    | Some board -> board
  in
  loop ()
