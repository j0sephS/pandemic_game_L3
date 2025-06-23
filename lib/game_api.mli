open Player
open Board
open City

type game_api
type settings

val create_settings : int -> int -> string -> settings
val get_nb_turns : game_api -> int
val get_current_player : game_api -> player
val get_game_state : game_api -> game_state
val start_game : city CityMap.t -> player list -> string -> game_state
val new_game : settings -> game_api
val update_game : game_api -> game_state -> game_api
val get_status : game_api -> bool
