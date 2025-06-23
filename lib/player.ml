open Card
open City
open Display

type player = {
  player_name : string;
  player_id : string;
  hand : card list;
  current_city : city;
  actions_left : int;
  isbot : bool;
}

(* constructors *)

let init_player name id h city actions b =
  {
    player_name = name;
    player_id = id;
    hand = h;
    current_city = city;
    actions_left = actions;
    isbot = b;
  }

let create_player name isbot (map : city CityMap.t) =
  let id =
    string_of_int (Oo.id (object end))
    (* better than random, real uuid *)
  and hand = []
  and city = get_city "Atlanta" map
  and actions = 4 in
  init_player name id hand city actions isbot

(* getters *)
let get_name player = player.player_name
let get_player_id player = player.player_id
let get_hand player = player.hand
let get_current_city player = player.current_city
let get_actions_left player = player.actions_left
let player_is_bot player = player.isbot

(* setters *)

let decrement_actions player =
  { player with actions_left = player.actions_left - 1 }

let set_hand player h = { player with hand = h }

(* print *)

let pphand fmt p =
  if List.length p.hand = 0 then Format.fprintf fmt "Empty."
  else List.iter (fun c -> Format.printf "%a" ppcard c) p.hand

let ppplayer fmt p =
  add_ansi_marking Format.std_formatter;
  Format.fprintf fmt "Player %s :@." p.player_name;
  Format.fprintf fmt "Is in : %a@." ppcity p.current_city;
  Format.fprintf fmt "Actions left : @{<fg_red>%d@}@." p.actions_left;
  Format.fprintf fmt "Hand : %a@." pphand p

(* Event Card *)

let player_is_on_city_of_card (p : player) (c : card) (map : city CityMap.t) :
    bool =
  city_equals p.current_city (get_city_of_card c map)

(* getters : *)

let get_card_of_city_name city_name player =
  try List.find (fun card -> get_city_name_of_card card = city_name) player.hand
  with Not_found ->
    Format.printf "Not found@.";
    raise Not_found

let getCardCurrentCity (p : player) (map : city CityMap.t) : card option =
  let rec getPotentialCard (myHand : card list) : card option =
    match myHand with
    | [] -> None
    | c :: xs ->
        if player_is_on_city_of_card p c map then Some c
        else getPotentialCard xs
  in
  getPotentialCard p.hand

(* Misc *)

let remove_card_hand (p : player) (c : card) : card list =
  List.filter (fun card -> not (card_equals card c)) p.hand

(* Booealan *)
(*test if a player can move to a city c next to him  *)

let is_city_neigboors_player (p : player) (c : city) : bool =
  let player_city_name = get_city_name p.current_city
  and city_neigboors = get_neighbors c in
  List.mem player_city_name city_neigboors

let player_contains_card_of_current_city (p : player) (cities : city CityMap.t)
    : bool =
  match getCardCurrentCity p cities with None -> false | Some _ -> true

let player_contains_card_c (p : player) (c : card) : bool = List.mem c p.hand
let player_can_make_an_action (p : player) : bool = p.actions_left > 0

let can_treat_disease (p : player) (c : string) : bool =
  get_cube_colors p.current_city c > 0
  && p.actions_left > 0
  && List.length p.hand <= 7

let player_can_move_by_car (p : player) (c : city) : bool =
  player_can_make_an_action p
  && is_city_neigboors_player p c
  && List.length p.hand <= 7

let player_can_fly (p : player) (c : card) : bool =
  player_can_make_an_action p
  && player_contains_card_c p c && card_is_player c
  && List.length p.hand <= 7

let player_can_charter_fly (p : player) (c : card) : bool =
  let nameCity1 = get_card_city_name c
  and nameCity2 = get_city_name p.current_city in
  player_can_fly p c && nameCity1 = nameCity2 && List.length p.hand <= 7

let can_construct_research_station (p : player) (cities : city CityMap.t) : bool
    =
  player_can_make_an_action p
  && player_contains_card_of_current_city p cities
  && (not (get_research_station p.current_city))
  && List.length p.hand <= 7

let can_move_by_shuttle (p : player) (city_research_stations : city CityMap.t)
    (city : city) : bool =
  player_can_make_an_action p
  && CityMap.cardinal city_research_stations >= 2
  && get_research_station p.current_city
  && CityMap.mem (get_city_name city) city_research_stations
  && (not (city_equals p.current_city city))
  && List.length p.hand <= 7

let player_can_fly (p : player) (c : card) : bool =
  player_can_make_an_action p
  && List.mem c p.hand && card_is_player c
  && List.length p.hand <= 7

let player_can_discover_remedy (p : player) (alreadyRemed : bool)
    (color : string) : card list * bool =
  if
    alreadyRemed
    || (not (get_research_station p.current_city))
    || List.length p.hand > 7
  then (p.hand, false)
  else
    let rec aux hand newHand cpt =
      match hand with
      | [] -> if cpt = 5 then (newHand, true) else (p.hand, false)
      | x :: xs ->
          Format.printf "%s@." (get_card_color x);
          if cpt = 5 then aux xs (newHand @ [ x ]) cpt
          else if get_card_color x = color then aux xs newHand (cpt + 1)
          else aux xs (newHand @ [ x ]) cpt
    in
    aux p.hand [] 0

let player_can_share_card p1 p2 : bool =
  city_equals p1.current_city p2.current_city && player_can_make_an_action p1

(* Player edit*)

let player_edit (p : player) (predicate : bool) (c : city) (h : card list) :
    player * bool =
  match predicate with
  | true ->
      ( { p with current_city = c; actions_left = p.actions_left - 1; hand = h },
        true )
  | false -> (p, false)

let player_move_by_car (p : player) (c : city) : player * bool =
  player_edit p (player_can_move_by_car p c && List.length p.hand <= 7) c p.hand

let player_move_by_direct_flight (p : player) (c : card) (map : city CityMap.t)
    : player * bool =
  player_edit p
    (player_can_fly p c && List.length p.hand <= 7)
    (get_city_of_card c map) (remove_card_hand p c)

let player_move_by_charter_flight (p : player) (city : city)
    (map : city CityMap.t) : player * bool =
  match getCardCurrentCity p map with
  | None -> (p, false)
  | Some c ->
      player_edit p
        (player_can_charter_fly p c && List.length p.hand <= 7)
        city (remove_card_hand p c)

let player_move_by_shuttle (p : player) (city : city) (cities : city CityMap.t)
    : player * bool =
  let l_research_station = get_research_station_city cities in
  player_edit p
    (can_move_by_shuttle p l_research_station city && List.length p.hand <= 7)
    city p.hand

let player_add_card p c =
  if List.mem c p.hand then (p, false)
  else (set_hand p (List.cons c p.hand), true)

(* Player others actions *)
(* add max 6 *)
let construct_research_station (p : player) (cities : city CityMap.t) :
    player * bool =
  if CityMap.cardinal (get_research_station_city cities) >= 6 then (p, false)
  else
    match getCardCurrentCity p cities with
    | None -> (p, false)
    | Some c ->
        let city = p.current_city in
        if not (get_research_station city) then (
          set_research_station city true;
          player_edit p true city (remove_card_hand p c))
        else (p, false)

(* last bool if a the player who received the card has more than 7*)
let p_give_share_to_p (p1 : player) (p2 : player) (cities : city CityMap.t)
    (give : bool) : player * player * bool * bool =
  match getCardCurrentCity p1 cities with
  | None -> if give then (p1, p2, false, false) else (p2, p1, false, false)
  | Some c ->
      let newHandp1 =
        List.filter
          (fun c -> not (get_card_city_name c = get_city_name p2.current_city))
          p1.hand
      and newHandp2 = p2.hand @ [ c ] in
      let p1' = { p1 with hand = newHandp1 } in
      let p1' = decrement_actions p1' and p2' = { p2 with hand = newHandp2 } in
      let b' = List.length p2'.hand > 7 in
      if give then (p1', p2', true, b') else (p2', p1', true, b')

let p1_give_card_to_p2 (p1 : player) (p2 : player) (cities : city CityMap.t) :
    player * player * bool * bool =
  if not (player_can_share_card p1 p2) then (p1, p2, false, false)
  else p_give_share_to_p p1 p2 cities true

let p1_take_card_to_p2 (p1 : player) (p2 : player) (cities : city CityMap.t) :
    player * player * bool * bool =
  if not (player_can_share_card p1 p2) then (p1, p2, false, false)
  else p_give_share_to_p p2 p1 cities false

(* match getCardCurrentCity p1 with
   | None -> (p1, p2, false,false)
   | Some c ->
       let l =
         List.filter
           (fun c -> not (get_card_city_name c = get_city_name p2.current_city))
           p1.hand
       in
       let p1', b =
         player_edit p1 (player_can_share_card p1 p2) p1.current_city l
       in
       if not b then (p1, p2, false)
       else
         let l2 = p2.hand @ [ c ] in
         let p2', b = player_edit p2 true p2.current_city l2 in
         let p2' = { p2' with actions_left = 4 } in
         (p1', p2', b) *)

let update_player_list_for_airlift (player_list : player list) (player : player)
    (player_to_move : player) (card : card) (city : city) : player list =
  List.map
    (fun x ->
      if get_player_id x = get_player_id player then
        { x with hand = remove_card_hand player card }
      else if get_player_id x = get_player_id player_to_move then
        { x with current_city = city }
      else x)
    player_list

let remove_card_from_player (player_list : player list) (player : player)
    (card : card) : player list =
  List.map
    (fun x ->
      if get_player_id x = get_player_id player then
        { x with hand = remove_card_hand player card }
      else x)
    player_list

let player_pass_turn (p : player) : player = { p with actions_left = 0 }
let set_next_turn_player (p : player) : player = { p with actions_left = 4 }

let player_discard (p : player) (c : string) : player * bool =
  let new_hand =
    List.filter (fun card -> not (card_equals_string card c)) p.hand
  in
  if List.length new_hand = List.length p.hand then (p, false)
  else ({ p with hand = new_hand }, true)
