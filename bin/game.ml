open Pandemicapi.Board
open Pandemicapi.Player
open Pandemicapi.City
open Pandemicapi.Display
open Pandemicapi.Game_api
open Pandemicapi.Bot
open Command

(* type state = { board : board; current_player : player } *)

(* let create_state board current_player = { board; current_player } *)

let rec create_game () =
  add_ansi_marking Format.std_formatter;
  (*ask for player numbers*)
  Format.printf "How many players (2-4) ?@.";
  let num_players = read_int () in
  if num_players < 2 || num_players > 5 then create_game ()
  else
    (*ask for player names*)
    let map = create_map () in
    let rec get_player_names num_players =
      if num_players = 0 then []
      else
        let () = Format.printf "Enter player name@." in
        let name = read_line () in
        let player = create_player name false map in
        player :: get_player_names (num_players - 1)
    in
    let players = get_player_names num_players in
    Format.printf
      "Which difficulty@ do you want@ to play@ (Easy,@ Standard,@ Heroic) ?@.";

    let difficulty = read_line () in
    if difficulty = "Easy" || difficulty = "Standard" || difficulty = "Heroic"
    then start_game map players difficulty
    else
      let () = Format.printf "Wrong@ difficulty" in
      create_game ()

(* Event card *)
(* let one_quit_night state = create_state state.board state.current_player *)

let rec turn player board =
  Format.printf "@.%a@." ppplayer player;
  if get_actions_left player = 0 then
    (* At the end of the turn of a player: *)
    (* draw 2 player cards *)
    (*if it's not a epidemic card discard one*)
    let new_player, new_board = draw_player_card board player in
    let new_player', new_board' = draw_player_card new_board new_player in
    (* infect cities *)
    (* update the state of the board *)
    player_update (end_turn_draw new_board') new_player'
  else
    let new_board =
      if player_is_bot player then bot_make_action board player
      else command_line_display board player
    in
    turn (update_player_with_board new_board player) new_board

let run_game board =
  let rec game_loop board turn_nbr =
    Format.printf "Turn %d@." turn_nbr;
    let rec turns players board =
      match players with
      (* If there is no players left for this turn return the current state of the board *)
      | [] -> board
      (* If there is atleast one to player that has not played during the current turn, make him play
         and update the state of the board for the next turns *)
      | h :: t -> (
          let g_state = turn (update_player_with_board board h) board in
          match g_state with Running _ -> turns t g_state | _ -> g_state)
    in
    match board with
    | Running board_running -> (
        match turns (get_player_list board_running) board with
        (* If the turns function returned Lost it means that the game is Lost, therefore return false *)
        | Lose s ->
            Format.printf "@{<fg_red>You lost : %s@}@." s;
            false
        (* If the turns function returned Won it means that the game is won, therefore return true *)
        | Won ->
            Format.printf "@{<fg_green>You won !!@}@.";
            true
        (* If the turns function returned something then run the game loop again with the new state of the board and increase turn_nbr *)
        | Running b -> game_loop (set_next_turn_board (Running b)) (turn_nbr + 1)
        )
    | Lose s ->
        Format.printf "@{<fg_red>You lost : %s@}@." s;
        false
    | Won ->
        Format.printf "@{<fg_green>You won !!@}@.";
        true
  in
  game_loop board 0
