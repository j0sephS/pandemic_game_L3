open Pandemicapi.Board

(* val create_state : board -> player -> state *)
val create_game : unit -> game_state
val run_game : game_state -> bool
