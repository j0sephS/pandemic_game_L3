open Game_api
open Board
open Player

(* [start_game city_map player_list difficulty_level] is the initial board state for a game of
 * Pandemic. *)
val call_action : game_api -> string -> string -> string -> game_api
val show_city_info : game_api -> string -> unit
val action_resilient_population : game_state -> player -> string -> game_state
val action_forecast : game_state -> player -> game_state
val action_one_quiet_night : game_state -> player -> game_state
val action_airlift : game_state -> player -> player -> string -> game_state
val action_government_grant : game_state -> player -> string -> game_state
val action_discard : game_state -> player -> string -> game_state
val pass_turn : game_state -> player -> game_state
val action_discover_remedy : game_state -> player -> string -> game_state
val action_treat_disease : game_state -> player -> string -> game_state
val action_construct_research_station : game_state -> player -> game_state
val action_move_by_shuttle : game_state -> player -> string -> game_state
val action_move_by_direct_flight : game_state -> player -> string -> game_state
val action_move_by_charter_flight : game_state -> player -> string -> game_state
val action_move_by_car : game_state -> player -> string -> game_state
val make_a_action : game_state -> player * bool -> game_state

val call_action_bot :
  game_api -> (game_state -> player -> game_state) -> game_api
