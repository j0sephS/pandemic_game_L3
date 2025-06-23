open City

(* type *)
type card
type typeCards

(* constructor *)

val create_cards : (string * city) list -> typeCards -> card list
val create_infection_cards : (string * city) list -> card list
val create_players_cards : (string * City.city) list -> card list
val init_card : string -> string -> string -> card
val create_event_cards : string -> card

(* print *)

val ppcard : Format.formatter -> card -> unit
val print_card_list : card list -> unit

(* getters *)

val get_card_color : card -> string
val get_card_city_name : card -> string
val get_city_of_card : card -> city CityMap.t -> city
val get_city_name_of_card : card -> string
val find_card : card list -> string -> card option
val get_type_card : card -> string

(* Booleans *)
val card_is_player : card -> bool
val card_is_event : card -> bool
val card_equals : card -> card -> bool
val get_card_event_name : card -> string
val create_epidemic_and_event_cards : card list -> string -> card list
val card_is_epidemic : card -> bool
val card_equals_string : card -> string -> bool
val split_list : 'a list -> int -> 'a list list
val shuffle_list : card list -> card list
