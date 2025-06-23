open Pandemicapi.Game_api
open Pandemicapi.Action
open Pandemicapi.Bot
(* let _ = run_game (create_game ())  *)
(* let map = create_map ()
   let player = create_player "Test" map
   let player2 = create_player "Test2" map
   let _ = start_game map ([ player ] @ [ player2 ]) "Easy" *)

let _ =
  let settings = create_settings 0 2 "Easy" in
  let game = new_game settings in
  let rec aux game =
    let game = call_action_bot game bot_make_action in
    if get_status game = true then aux game else false
  in
  aux game

(* let _ =
   let settings = create_settings 2 "Easy" in
   let game = new_game settings in
   let game = call_action game "Player 1" "next_turn" "" in
   let game = call_action game "Player 2" "next_turn" "" in
   let game = call_action game "Player 1" "next_turn" "" in
   let _ = call_action game "Player 2" "next_turn" "" in
   () *)
