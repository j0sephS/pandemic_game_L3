open Card
open City

type player

val pphand : Format.formatter -> player -> unit
val ppplayer : Format.formatter -> player -> unit
val create_player : string -> bool -> city CityMap.t -> player
val get_current_city : player -> city
val get_player_id : player -> string
val get_name : player -> string
val get_hand : player -> card list
val get_card_of_city_name : string -> player -> card
val get_actions_left : player -> int
val is_city_neigboors_player : player -> city -> bool
val player_can_make_an_action : player -> bool
val player_can_move_by_car : player -> city -> bool
val player_can_fly : player -> card -> bool
val player_move_by_car : player -> city -> player * bool
val set_hand : player -> card list -> player

val player_move_by_direct_flight :
  player -> card -> city CityMap.t -> player * bool

val player_move_by_charter_flight :
  player -> city -> city CityMap.t -> player * bool

val player_add_card : player -> card -> player * bool
val remove_card_hand : player -> card -> card list
val player_can_charter_fly : player -> card -> bool
val decrement_actions : player -> player
val player_is_bot : player -> bool

(* val player_is_on_city_of_card : player -> card -> city CityMap.t -> bool *)
val can_construct_research_station : player -> city CityMap.t -> bool
val can_treat_disease : player -> string -> bool

(* val treat_disease :
   player -> remedies -> color -> player * remedies * cubes * bool *)

val construct_research_station : player -> city CityMap.t -> player * bool
val player_contains_card_of_current_city : player -> city CityMap.t -> bool
val player_contains_card_c : player -> card -> bool
val getCardCurrentCity : player -> city CityMap.t -> card option
val player_edit : player -> bool -> city -> card list -> player * bool
val player_move_by_shuttle : player -> city -> city CityMap.t -> player * bool
val can_move_by_shuttle : player -> city CityMap.t -> city -> bool
val player_can_discover_remedy : player -> bool -> string -> card list * bool

val p1_give_card_to_p2 :
  player -> player -> city CityMap.t -> player * player * bool * bool

val p1_take_card_to_p2 :
  player -> player -> city CityMap.t -> player * player * bool * bool

val update_player_list_for_airlift :
  player list -> player -> player -> card -> city -> player list

val remove_card_from_player : player list -> player -> card -> player list
val player_pass_turn : player -> player
val set_next_turn_player : player -> player
val player_discard : player -> string -> player * bool
