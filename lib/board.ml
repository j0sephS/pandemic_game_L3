open City
open Player
open Card

(* board of the game *)
type remedies = {
  blue : bool * bool;
  red : bool * bool;
  yellow : bool * bool;
  black : bool * bool;
}

type board = {
  cities : city CityMap.t;
  players : player list;
  infection_rate : int;
  infection_deck : card list;
  infection_discard : card list;
  player_deck : card list;
  outbreak_count : int;
  cubes_stock : int list;
  remedy : remedies;
  one_quiet_night : bool;
}

type game_state = Lose of string | Won | Running of board
(* function to create the board *)

let init_remedies =
  {
    blue = (false, false);
    red = (false, false);
    yellow = (false, false);
    black = (false, false);
  }

let create_board map players_list card_infection_list card_player_list =
  {
    cities = map;
    players = players_list;
    infection_rate = 0;
    infection_deck = card_infection_list;
    infection_discard = [];
    player_deck = card_player_list;
    outbreak_count = 0;
    cubes_stock = [ 24; 24; 24; 24 ];
    remedy = init_remedies;
    one_quiet_night = false;
  }

(* getters *)
let get_cities b = b.cities
let get_player_deck b = b.player_deck
let get_player_list b = b.players
let get_cube_stock b = b.cubes_stock
let get_outbreak_count b = b.outbreak_count
let get_infection_discard b = b.infection_discard
let get_infection_deck b = b.infection_deck
let get_one_quiet_night b = b.one_quiet_night

(* setters *)

let delete_one_quiet_night board = { board with one_quiet_night = false }

(* update the player with id of player_id *)
let player_update board player_updated =
  match board with
  | Running b ->
      Running
        {
          b with
          players =
            List.map
              (fun player ->
                if get_player_id player = get_player_id player_updated then
                  player_updated
                else player)
              b.players;
        }
  | s -> s

let draw_2_card board =
  if List.length board.player_deck < 2 then raise (Failure "player deck empty")
  else
    let c1 = List.hd board.player_deck in
    let c2 = List.nth board.player_deck 1 in
    let new_board =
      { board with player_deck = List.tl (List.tl board.player_deck) }
    in
    (new_board, c1, c2)

let get_remedies (r : remedies) (c : string) : bool * bool =
  match convert_string_to_color c with
  | 0 -> r.blue
  | 1 -> r.red
  | 2 -> r.yellow
  | 3 -> r.black
  | _ -> raise (Failure "color not found")

let get_stock_cube_of_color b c =
  let i = convert_string_to_color c in
  List.nth b.cubes_stock i

let convert_infection_to_numbers_of_card board =
  match board.infection_rate with
  | 0 | 1 | 2 -> 2
  | 3 | 4 -> 3
  | 5 | 6 -> 4
  | _ -> raise (Failure "bad infection rate")

(* booleans *)
let remed_is_discovered (r : remedies) (c : string) : bool =
  let d, _ = get_remedies r c in
  d

let disease_is_eradicated (r : remedies) (c : string) : bool =
  let _, e = get_remedies r c in
  e

(* setters *)
let set_remedies (r : remedies) (c : string) (b : bool * bool) : remedies =
  match convert_string_to_color c with
  | 0 -> { r with blue = b }
  | 1 -> { r with red = b }
  | 2 -> { r with yellow = b }
  | 3 -> { r with black = b }
  | _ -> raise (Failure "color not found")

let set_city_cube board city color n =
  CityMap.add (get_city_name city) (set_cube city color n) board.cities

let remed_discover (r : remedies) (c : string) : remedies =
  set_remedies r c (true, false)

let set_infection_rate board n : board =
  if n >= 7 then { board with infection_rate = 6 }
  else { board with infection_rate = n }

let increment_infection_rate board =
  set_infection_rate board (board.infection_rate + 1)

let disease_eradicate (r : remedies) (c : string) : remedies =
  set_remedies r c (true, true)

let modify_cube_stock cube_stock c n =
  if n <= 0 then raise (Failure "Empty cube stock (game lost)")
  else
    let i = convert_string_to_color c in
    let rec aux l j =
      if j = 4 then l
      else if i = j then aux (l @ [ n ]) (j + 1)
      else aux (l @ [ List.nth cube_stock j ]) (j + 1)
    in
    aux [] 0

let add_cube_stock b c n =
  modify_cube_stock b.cubes_stock c (get_stock_cube_of_color b c + n)

let remove_cube_stock (b : board) (c : string) (n : int) : int list =
  modify_cube_stock b.cubes_stock c (get_stock_cube_of_color b c - n)

(* Action *)

let discover_remedy (board : board) (p : player) (color : string) :
    player * remedies * bool =
  let newHand, b =
    player_can_discover_remedy p (remed_is_discovered board.remedy color) color
  in
  if not b then (p, board.remedy, false)
  else
    let p', _ = player_edit p true (get_current_city p) newHand in
    if get_stock_cube_of_color board color = 24 then
      (p', disease_eradicate board.remedy color, true)
    else (p', remed_discover board.remedy color, true)

let all_remedies_discovered (r : remedies) : bool =
  remed_is_discovered r "blue"
  && remed_is_discovered r "red"
  && remed_is_discovered r "yellow"
  && remed_is_discovered r "black"

let discover_remedy_simplify (board : board) (p : player) (color : string) :
    player * game_state * bool =
  let new_player, new_remedies, possible = discover_remedy board p color in
  if not possible then (p, Running board, false)
  else if all_remedies_discovered new_remedies then (new_player, Won, true)
  else
    let new_board = { board with remedy = new_remedies } in
    (new_player, Running new_board, true)

let treat_disease (board : board) (p : player) (c : string) :
    player * remedies * int list * city * bool =
  let b = can_treat_disease p c in
  if not b then (p, board.remedy, board.cubes_stock, get_current_city p, false)
  else
    let city', cube' =
      if remed_is_discovered board.remedy c then
        let nbCube = get_cube_colors (get_current_city p) c in
        let city' = set_cube (get_current_city p) c 0 in
        let cube' = add_cube_stock board c nbCube in
        (city', cube')
      else
        let city' = discrease_cube (get_current_city p) c in
        let cube' = add_cube_stock board c 1 in
        (city', cube')
    in
    let remedies' =
      if
        get_stock_cube_of_color board c = 24
        && remed_is_discovered board.remedy c
      then disease_eradicate board.remedy c
      else board.remedy
    in
    let p', _ = player_edit p true city' (get_hand p) in
    (p', remedies', cube', city', true)

let treat_disease_simplify (board : board) (p : player) (color : string) =
  let new_player, new_remedies, new_cube_stock, new_city, possible =
    treat_disease board p color
  in
  if not possible then (p, board, false)
  else
    let new_board =
      {
        board with
        cities = CityMap.add (get_city_name new_city) new_city board.cities;
        cubes_stock = new_cube_stock;
        remedy = new_remedies;
      }
    in
    (new_player, new_board, true)

(* function to print the board *)
let print_player_list player_list =
  let rec aux player_list =
    match player_list with
    | [] -> ()
    | h :: q ->
        Format.printf "%a" ppplayer h;
        aux q
  in
  aux player_list

let print_board board =
  Format.printf "Cities:@.";
  print_map board.cities;
  Format.printf "Players:@.";
  print_player_list board.players;
  Format.printf "Infection rate: %d@."
    (convert_infection_to_numbers_of_card board);
  Format.printf "Infection discard:@.";
  print_card_list board.infection_discard;
  Format.printf "Outbreak count: %d@." board.outbreak_count

let shuffle liste =
  let rec get_from_list index liste acc =
    match index with
    | 0 -> (List.hd liste, List.rev_append acc (List.tl liste))
    | _ -> get_from_list (index - 1) (List.tl liste) (List.hd liste :: acc)
  in
  Format.printf "Rearranging@ the 6@ first@ cards@ of@ the@ infection@ deck:@.";
  let rec aux acc l =
    if List.length l = 0 then List.rev acc
    else (
      Format.printf "Cards@ in@ the@ deck:@.";
      List.iter (fun x -> Format.printf "%a" ppcard x) l;
      Format.printf
        "Enter@ the@ index@ of@ the@ card@ to@ pick (starts at 0 at the top):@.";
      Format.printf ">%!";
      let index_out = read_int_opt () in
      match index_out with
      | None ->
          Format.printf "Invalid@ index.@ Please@ enter@ a@ valid@ index.@.";
          aux acc l
      | Some index_out -> (
          if index_out < 0 || index_out > List.length l - 1 then (
            Format.printf "Invalid@ index.@ Please@ enter@ a@ valid@ index.@.";
            aux acc l)
          else
            match get_from_list index_out l [] with
            | c, remaining -> aux (c :: acc) remaining))
  in
  aux [] liste

let rec draw deck liste index =
  match index with
  | 0 -> (List.rev liste, deck)
  | _ -> (
      match deck with
      | [] -> raise (Failure "deck empty")
      | h :: q -> draw q (h :: liste) (index - 1))

let rec decrease_cube_stock (cubes_stock : int list) (color : int) : int list =
  match cubes_stock with
  | [] -> raise (Failure "color doesn't exist")
  | h :: q ->
      if color = 0 then (h - 1) :: q else h :: decrease_cube_stock q (color - 1)

let add_infection_cube_to_city (board : board) (city : city) (color : string) :
    game_state =
  let rec add_infection_cube_to_city_helper (board : board) (city : city)
      (color : string) (contamined : city list) : game_state =
    let rec propagation (board : game_state) (neighbors : string list)
        (color : string) (contamined : city list) : game_state =
      match board with
      | Running board_running -> (
          match neighbors with
          | [] -> board
          | h :: q ->
              let city = get_city h board_running.cities in
              if List.mem city contamined then
                propagation board q color contamined
              else
                propagation
                  (add_infection_cube_to_city_helper board_running city color
                     contamined)
                  q color (city :: contamined))
      | s -> s
    in
    if List.nth board.cubes_stock (convert_string_to_color color) = 0 then
      Lose ("No more cube of " ^ color)
    else if cube_3_or_more city then (
      let city_neighbors = get_neighbors city in
      Format.printf "Eclosion@ on@ %a@." ppcity city;
      Format.printf "outbreak_count : %d@." board.outbreak_count;
      if board.outbreak_count + 1 = 8 then Lose "8 outbreaks occured"
      else
        propagation
          (Running { board with outbreak_count = board.outbreak_count + 1 })
          city_neighbors color ([ city ] @ contamined))
    else
      let new_stock =
        decrease_cube_stock board.cubes_stock (convert_string_to_color color)
      in
      let new_map =
        CityMap.add (get_city_name city) (add_cube city color)
          board.cities (*add can replace optimize Map.update*)
      in
      Running { board with cubes_stock = new_stock; cities = new_map }
  in
  add_infection_cube_to_city_helper board city color []

let end_turn_draw (board : game_state) : game_state =
  match board with
  | Running board_running -> (
      let rec infection_phase (board : game_state) (cards : card list) :
          game_state =
        match board with
        | Running board_running -> (
            match cards with
            | [] -> board
            | h :: q ->
                let city = get_city_of_card h board_running.cities in
                infection_phase
                  (add_infection_cube_to_city board_running city
                     (get_city_color city))
                  q)
        | s -> s
      in
      let board =
        if
          List.length board_running.infection_deck
          <= convert_infection_to_numbers_of_card board_running
        then
          {
            board_running with
            infection_deck =
              shuffle_list
                (board_running.infection_deck @ board_running.infection_discard);
            infection_discard = [];
          }
        else board_running
      in
      let draws, deck =
        draw board.infection_deck []
          (convert_infection_to_numbers_of_card board)
      in
      Format.printf "Infection phase:@.";
      Format.printf "Cards drawed:@.";
      print_card_list draws;
      match infection_phase (Running board) draws with
      | Running b ->
          Running
            {
              b with
              infection_deck = deck;
              infection_discard = draws @ board.infection_discard;
            }
      | s -> s)
  | s -> s

let draw_last_card_deck deck : card * card list =
  let c = List.nth deck (List.length deck - 1) in
  let newDeck = List.filter (fun x -> not (card_equals c x)) deck in
  (c, newDeck)

let draw_infection_card_deck deck : card * card list =
  let c = List.hd deck in
  let newDeck = List.tl deck in
  (c, newDeck)

let acceleration board =
  match board with Running b -> Running (increment_infection_rate b) | s -> s

let infection board =
  match board with
  | Running board -> (
      let last_card, infection_deck' =
        draw_last_card_deck board.infection_deck
      in
      let color_last_card = get_card_color last_card in
      let board' =
        if not (disease_is_eradicated board.remedy color_last_card) then
          try
            let city_last_card = get_city_of_card last_card board.cities in
            let nbCubes = get_cube_colors city_last_card color_last_card in
            let stock = remove_cube_stock board color_last_card (3 - nbCubes) in
            let map = set_city_cube board city_last_card color_last_card 3 in
            let board'' =
              {
                board with
                cities = map;
                cubes_stock = stock;
                infection_deck = infection_deck';
              }
            in
            add_infection_cube_to_city board'' city_last_card color_last_card
          with Failure _ -> Lose ("No more cube of " ^ color_last_card)
        else Running board
      in
      match board' with
      | Running b ->
          Running
            { b with infection_discard = b.infection_discard @ [ last_card ] }
      | s -> s)
  | s -> s

let intensification board' =
  match board' with
  | Running b ->
      let infection_discard_shuffle = shuffle_list b.infection_discard in
      let infection_deck' = infection_discard_shuffle @ b.infection_deck in
      (*not edit the player deck for optimization it's then tail when draw*)
      let newBoard =
        { b with infection_deck = infection_deck'; infection_discard = [] }
      in
      Running newBoard
  | s -> s

let draw_epidemic board c : game_state * bool =
  if not (card_is_epidemic c) then (board, false)
  else
    let board_after_acceleration = acceleration board in
    let board_after_infection = infection board_after_acceleration in
    let board_after_intensification = intensification board_after_infection in
    (board_after_intensification, true)

let deck_with_epidemics_and_event board difficulty_level =
  {
    board with
    player_deck =
      create_epidemic_and_event_cards board.player_deck difficulty_level;
  }

let start_infection board =
  let board = set_infection_rate board 2 in
  let rec aux board count =
    if count = 0 then board
    else
      let card, deck = draw_infection_card_deck board.infection_deck in
      Format.printf "Infection on: %a@." ppcity
        (get_city_of_card card board.cities);
      let city = get_city_of_card card board.cities in
      let number_of_cubes_to_add = (count + 2) / 3 in
      aux
        {
          board with
          infection_deck = deck;
          cities =
            set_city_cube board city (get_city_color city)
              number_of_cubes_to_add;
        }
        (count - 1)
  in
  aux board 9

(* Event Card *)
let airlift player player_to_move (city : string) board : board =
  if CityMap.mem city (get_cities board) then
    match find_card (get_hand player) "Airlift" with
    | None ->
        Format.printf "Card not found";
        raise (Failure "Card not found")
    | Some c ->
        {
          board with
          players =
            update_player_list_for_airlift board.players player player_to_move c
              (CityMap.find city (get_cities board));
        }
  else (
    Format.printf "City not found";
    raise (Failure "City not found"))

let one_quiet_night board player =
  match find_card (get_hand player) "One Quiet Night" with
  | None -> failwith "Not found"
  | Some c ->
      {
        board with
        players = remove_card_from_player board.players player c;
        one_quiet_night = true;
      }

let resilient_population board player : game_state =
  match board with
  | Running board_running -> (
      match find_card (get_hand player) "Resilient Population" with
      | None -> failwith "Not found"
      | Some c ->
          Format.printf
            "Which@ card@ do@ you@ want@ to@ remove@ from@ the@ game?@.";
          print_card_list (get_infection_discard board_running);
          Format.printf ">%!";
          let card_name = read_line () in
          let rec aux card_name card_list =
            match card_list with
            | [] -> raise (Failure "Card not found")
            | h :: q ->
                if get_city_name_of_card h = card_name then q
                else h :: aux card_name q
          in
          let new_infection_discard =
            aux card_name (get_infection_discard board_running)
          in
          Running
            {
              board_running with
              players = remove_card_from_player board_running.players player c;
              infection_discard = new_infection_discard;
            })
  | s -> s

let resilient_population_not_interactive (board : board) (player : player)
    (card_name : string) : board =
  match find_card (get_hand player) "Resilient Population" with
  | None -> failwith "Not found"
  | Some c ->
      let rec aux card_name card_list =
        match card_list with
        | [] -> raise (Failure "Card not found")
        | h :: q ->
            if get_city_name_of_card h = card_name then q
            else h :: aux card_name q
      in
      let new_infection_discard = aux card_name (get_infection_discard board) in
      {
        board with
        players = remove_card_from_player board.players player c;
        infection_discard = new_infection_discard;
      }

let forecast_interactive player board : game_state =
  match board with
  | Running board_running -> (
      match find_card (get_hand player) "Forecast" with
      | None -> board
      | Some c ->
          if List.length (get_infection_deck board_running) >= 6 then
            let draws, deck = draw (get_infection_deck board_running) [] 6 in
            let draws_list = shuffle draws in

            (*find a way to do this in no interactive mode*)

            (* Crée une nouvelle liste d'infection_deck avec les 6 premières cartes remplacées par draws_list *)
            let new_infection_deck = draws_list @ deck in
            Running
              {
                board_running with
                players = remove_card_from_player board_running.players player c;
                infection_deck = new_infection_deck;
              }
          else raise (Failure "Not enough card in the infection deck"))
  | s -> s

let forecast player board : board =
  match find_card (get_hand player) "Forecast" with
  | None -> board
  | Some c ->
      if List.length (get_infection_deck board) >= 6 then
        let draws, deck = draw (get_infection_deck board) [] 6 in
        let draws_list = shuffle_list draws in

        (*random shuffle but need to be edited*)

        (* Crée une nouvelle liste d'infection_deck avec les 6 premières cartes remplacées par draws_list *)
        let new_infection_deck = draws_list @ deck in
        {
          board with
          players = remove_card_from_player board.players player c;
          infection_deck = new_infection_deck;
        }
      else board

let government_grant (player : player) (city_name : string) (board : board) :
    board =
  if CityMap.cardinal (get_research_station_city (get_cities board)) = 6 then
    board
  else
    match find_card (get_hand player) "Government Grant" with
    | None -> board
    | Some c ->
        {
          board with
          players = remove_card_from_player board.players player c;
          cities = set_research_station_to_city board.cities city_name;
        }

let draw_player_card board player =
  match board with
  | Running board_running -> (
      if List.length board_running.player_deck = 0 then
        (player, Lose "Player deck empty")
      else
        let c = List.hd board_running.player_deck in
        if card_is_epidemic c then
          let board, _ = draw_epidemic board c in
          match board with
          | Running b ->
              ( player,
                Running
                  { b with player_deck = List.tl board_running.player_deck } )
          | s -> (player, s)
        else
          let new_player, possible = player_add_card player c in
          if not possible then (new_player, Running board_running)
          else
            let new_board = player_update board new_player in
            match new_board with
            | Running b ->
                ( new_player,
                  Running { b with player_deck = List.tl b.player_deck } )
            | s -> (new_player, s))
  | s -> (player, s)

let set_next_turn_board board =
  match board with
  | Running b ->
      let new_player_list =
        List.map (fun p -> set_next_turn_player p) b.players
      in
      Running { b with players = new_player_list }
  | s -> s
