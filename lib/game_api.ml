open Board
open Player
open City
open Card

type game_api = {
  nb_turns : int;
  current_player : player;
  game_state : game_state;
  running : bool;
}

type settings = { nb_players : int; difficulty_level : string; nb_bots : int }

let create_settings nb1 nb2 difficulty =
  if nb1 + nb2 > 4 || nb1 < 0 || nb2 < 0 then
    raise (Failure "Wrong numbers of players")
  else { nb_players = nb1; difficulty_level = difficulty; nb_bots = nb2 }

let get_nb_turns game_api = game_api.nb_turns
let get_current_player game_api = game_api.current_player
let get_game_state game_api = game_api.game_state
let get_status game_api = game_api.running

let deal_player_cards (board : game_state) player number_player : game_state =
  let rec deal_player_cards_helper board player num_cards =
    if num_cards = 0 then board
    else
      let new_player, new_board = draw_player_card board player in
      match new_board with
      | Running board ->
          deal_player_cards_helper (Running board) new_player (num_cards - 1)
      | s -> s
  in
  deal_player_cards_helper board player (6 - number_player)

let start_game map player_list difficulty_level =
  if List.length player_list < 2 then
    failwith "Not enough players to start game"
  else if List.length player_list > 4 then
    failwith "Too many players to start game"
  else
    let infections_cards = create_infection_cards (CityMap.bindings map) in
    let player_cards = create_players_cards (CityMap.bindings map) in
    let board =
      create_board map player_list
        (shuffle_list infections_cards)
        (shuffle_list player_cards)
    in
    let number_player = List.length player_list in
    let rec deal_cards board player_list =
      match player_list with
      | [] -> board
      | h :: t -> deal_cards (deal_player_cards board h number_player) t
    in
    let board = deal_cards (Running board) player_list in
    match board with
    | Running board ->
        let board = deck_with_epidemics_and_event board difficulty_level in
        Running (start_infection board)
    | s -> s

let new_game (settings : settings) : game_api =
  let map = create_map () in
  let player_list =
    List.init settings.nb_players (fun i ->
        create_player ("Player " ^ string_of_int (i + 1)) false map)
    @ List.init settings.nb_bots (fun i ->
          create_player
            ("Player (bot) " ^ string_of_int (settings.nb_players + i + 1))
            true map)
  in
  let board = start_game map player_list settings.difficulty_level in
  match board with
  | Running board_running ->
      {
        nb_turns = 0;
        game_state = board;
        current_player = List.hd (get_player_list board_running);
        running = true;
      }
  | _ -> failwith "Invalid board"

let find_index list el =
  let rec aux l el acc =
    match l with
    | [] -> acc
    | h :: t ->
        if get_player_id h = get_player_id el then acc else aux t el (acc + 1)
  in
  aux list el 0

(* Si on a fait le tour de chaque joueur, (0,true) pour dire que le tour est fini
   Sinon (new_index, false) et on continue *)
let next_player player player_list =
  let new_index = find_index player_list player + 1 in
  if new_index = List.length player_list then (List.nth player_list 0, true)
  else (List.nth player_list new_index, false)

let update_game game new_board =
  match new_board with
  | Lose s ->
      Format.printf "Lose : %s@." s;
      { game with running = false }
  | Won ->
      Format.printf "You Win !!@.";
      { game with running = false }
  | Running new_board_running ->
      (* update player *)
      let new_player =
        List.find
          (fun p -> get_name p = get_name (get_current_player game))
          (get_player_list new_board_running)
      in
      if get_actions_left new_player = 0 then (
        Format.printf "You have no more actions left. Ending your turn...@.";

        (* draw infection cards *)
        let board_with_new_infection =
          if get_one_quiet_night new_board_running then
            Running (delete_one_quiet_night new_board_running)
          else end_turn_draw new_board
        in
        (* draw 2 player cards *)
        let new_p, new_b =
          draw_player_card board_with_new_infection new_player
        in
        let new_p', new_b' = draw_player_card new_b new_p in
        (* update board with updated player *)
        let board_updated = player_update new_b' new_p' in
        (* if last player in the list, go back to first and increase turn nbr *)
        let next_p, next_t =
          next_player new_player (get_player_list new_board_running)
        in
        if next_t then (
          Format.printf "End of turn %d@." game.nb_turns;
          {
            nb_turns = game.nb_turns + 1;
            current_player = set_next_turn_player next_p;
            game_state = set_next_turn_board board_updated;
            running = true;
          })
        else { game with game_state = board_updated; current_player = next_p }
        (*the information on win or lose come from the board stocked*))
      else { game with game_state = new_board; current_player = new_player }
