type city
type cubes
(* type color *)

module CityMap : Map.S with type key = string

val set_cube : city -> string -> int -> city
val discrease_cube : city -> string -> city
(* getters *)

(* val get_city_name : city -> string *)
val get_neighbors : city -> string list
val get_city : CityMap.key -> 'a CityMap.t -> 'a
val get_cube_colors : city -> string -> int
val get_color : city -> string
val get_cube : city -> cubes
val get_number_cube : city -> int
val has_cube : city -> string option
val city_in_map : city CityMap.t -> string -> bool

(* val edit_cube : cubes -> string -> bool -> cubes *)
val add_cube : city -> string -> city
val convert_string_to_color : string -> int
val test_string_color : string -> bool
val get_research_station_city : city CityMap.t -> city CityMap.t
val get_city_name : city -> string
val get_city_color : city -> string
val get_research_station : city -> bool
val set_research_station : city -> bool -> unit
val create_map : unit -> city CityMap.t
val print_map : city CityMap.t -> unit
val ppcity : Format.formatter -> city -> unit
val ppcity_full : Format.formatter -> city * city CityMap.t -> unit
val ppneighbors : Format.formatter -> city * city CityMap.t -> unit
val ppresearch_stations : Format.formatter -> city * city CityMap.t -> unit
val get_city_id : city -> int
val city_equals : city -> city -> bool
val cube_3_or_more : city -> bool
val set_research_station_to_city : city CityMap.t -> string -> city CityMap.t
