open City
open Player
open Card

(* Struct of board *)
type board
type remedies
type game_state = Lose of string | Won | Running of board

val create_board :
  city CityMap.t -> player list -> card list -> card list -> board

val print_board : board -> unit
val get_cities : board -> city CityMap.t
val get_player_deck : board -> card list
val get_player_list : board -> player list
val get_cube_stock : board -> int list
val get_outbreak_count : board -> int
val get_infection_discard : board -> card list
val get_infection_deck : board -> card list
val init_remedies : remedies
val discover_remedy : board -> player -> string -> player * remedies * bool

val discover_remedy_simplify :
  board -> player -> string -> player * game_state * bool

val remed_is_discovered : remedies -> string -> bool

val treat_disease :
  board -> player -> string -> player * remedies * int list * city * bool

val treat_disease_simplify : board -> player -> string -> player * board * bool

(*
val modify_remedies : remedies -> color -> bool * bool -> remedies
val get_remedies : remedies -> color -> bool * bool *)
val end_turn_draw : game_state -> game_state
val add_infection_cube_to_city : board -> city -> string -> game_state
val draw_epidemic : game_state -> card -> game_state * bool
val draw_player_card : game_state -> player -> player * game_state
val deck_with_epidemics_and_event : board -> string -> board
val start_infection : board -> board
val draw_2_card : board -> board * card * card
val player_update : game_state -> player -> game_state
val draw : card list -> card list -> int -> card list * card list
val shuffle : card list -> card list
val airlift : player -> player -> string -> board -> board
val resilient_population : game_state -> player -> game_state
val forecast_interactive : player -> game_state -> game_state
val forecast : player -> board -> board
val government_grant : player -> string -> board -> board
val set_next_turn_board : game_state -> game_state
val one_quiet_night : board -> player -> board
val delete_one_quiet_night : board -> board
val get_one_quiet_night : board -> bool
val resilient_population_not_interactive : board -> player -> string -> board
