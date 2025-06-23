open Action
open Player
open City
open Board
open Card

let get_good_city l =
  let rec aux l' =
    match l' with
    | [] -> None
    | c :: xs -> ( match has_cube c with None -> aux xs | Some _ -> Some c)
  in
  aux l

let get_city_plane board player =
  let card_hand = get_hand player in
  let rec aux l city_plane_neighbors =
    match l with
    | [] -> city_plane_neighbors
    | card :: xs ->
        if card_is_player card then
          let city =
            CityMap.find (get_card_city_name card) (get_cities board)
          in
          aux xs (city_plane_neighbors @ [ city ])
        else aux xs city_plane_neighbors
  in
  aux card_hand []

let get_city_neighbors board player =
  let current_city = get_current_city player in
  let neighbors = get_neighbors current_city in
  let rec aux l city_neighbors =
    match l with
    | [] -> city_neighbors
    | v :: xs ->
        let c = CityMap.find v (get_cities board) in
        aux xs (city_neighbors @ [ c ])
  in
  aux neighbors []

let bot_good_travel board player =
  match board with
  | Running b -> (
      let l = get_city_neighbors b player in
      match get_good_city l with
      | Some city -> Some (action_move_by_car board player (get_city_name city))
      | None -> (
          let l = get_city_plane b player in
          match get_good_city l with
          | Some city ->
              Some
                (action_move_by_direct_flight board player (get_city_name city))
          | None -> None))
  | _ -> None

let bot_good_treatment board player =
  let current_city = get_current_city player in
  match has_cube current_city with
  | None -> None
  | Some color -> Some (action_treat_disease board player color)

(* enleve la carte dans la main dont la couelur est le minimum *)
let rec good_draw l =
  if List.length l <= 7 then l
  else
    let rec aux l' red blue yellow black =
      match l' with
      | [] -> (red, blue, yellow, black)
      | c :: xs -> (
          if not (card_is_player c) then aux xs red blue yellow black
          else
            match get_card_color c with
            | "red" -> aux xs (red @ [ c ]) blue yellow black
            | "blue" -> aux xs red (blue @ [ c ]) yellow black
            | "black" -> aux xs red blue yellow (black @ [ c ])
            | "yellow" -> aux xs red blue (yellow @ [ c ]) black
            | _ -> aux xs red blue yellow black)
    in
    let redCard, blueCard, yellowCard, blackCard = aux l [] [] [] [] in
    let lenRed, lenBlue, lenBlack, lenYellow =
      ( List.length redCard,
        List.length blueCard,
        List.length blackCard,
        List.length yellowCard )
    in
    let len =
      [
        (lenRed, "red");
        (lenBlue, "blue");
        (lenBlack, "black");
        (lenYellow, "yellow");
      ]
    in
    let len = List.filter (fun (a, _) -> a <> 0) len in
    let min_nbr, min_color =
      List.hd (List.sort (fun (a, _) (b, _) -> a - b) len)
    in
    if List.length l - min_nbr < 7 then
      let number_of_card_to_take_from_min_color =
        7 - (List.length l - min_nbr)
      in
      let take_x_card =
        let rec aux acc card_list = function
          | [] -> card_list
          | h :: t ->
              if acc = number_of_card_to_take_from_min_color then card_list
              else aux (acc + 1) (h :: card_list) t
        in
        aux 0 []
      in
      List.filter (fun c -> get_card_color c <> min_color) l
      @ take_x_card (List.filter (fun c -> get_card_color c = min_color) l)
    else good_draw (List.filter (fun c -> get_card_color c <> min_color) l)

let good_draw_simplify l =
  let hand_without_events = List.filter (fun c -> card_is_player c) l in
  if List.length hand_without_events <= 7 then hand_without_events
  else good_draw hand_without_events

(* let redCard = List.filter (fun c -> get_card_color c = "red") l and
   blueCard = List.filter (fun c -> get_card_color c = "blue") l and
   blackCard = List.filter (fun c -> get_card_color c = "black") l and
   yellowCard = List.filter (fun c -> get_card_color c = "yellow") l in *)

let rec need_to_draw_cards board player =
  if List.length (get_hand player) <= 7 then player
  else
    let newHand = good_draw_simplify (get_hand player) in
    need_to_draw_cards board (set_hand player newHand)

let contains_event_card player =
  let f c = card_is_event c in
  match List.find_opt f (get_hand player) with
  | None -> player
  | Some _ ->
      let newhand =
        List.filter (fun x -> not (card_is_event x)) (get_hand player)
      in
      set_hand player newhand

(* let use_event_card player board c =
   match get_card_event_name c with
   | "Forecast" -> action_forecast board player
   | "Airlift" -> action_airlift board player "Bagdad"
   | "Government Grant" -> action_government_grant board player "Bagdad"
   | "Resilient Population" ->
     let c = List.nth (get_infection_discard board) 0 in
     action_resilient_population board player (get_card_city_name c)
   | "One Quiet Night" -> action_one_quiet_night board player
   | _ -> board *)
let have_the_card_of_the_city city player =
  let city_name = get_city_name city in
  List.exists (fun c -> get_card_city_name c = city_name) (get_hand player)

let get_the_color_of_the_5_cards player =
  let redCard =
    List.filter (fun c -> get_card_color c = "red") (get_hand player)
  and blueCard =
    List.filter (fun c -> get_card_color c = "blue") (get_hand player)
  and blackCard =
    List.filter (fun c -> get_card_color c = "black") (get_hand player)
  and yellowCard =
    List.filter (fun c -> get_card_color c = "yellow") (get_hand player)
  in
  if List.length redCard >= 5 then "red"
  else if List.length blueCard >= 5 then "blue"
  else if List.length blackCard >= 5 then "black"
  else if List.length yellowCard >= 5 then "yellow"
  else ""

let bot_make_action board player =
  (* ajouter si 7 carte discard 1 *)
  let player = need_to_draw_cards board player in
  let player = contains_event_card player in
  match bot_good_treatment board player with
  | Some board' -> board'
  | None -> (
      match bot_good_travel board player with
      | Some board' -> board'
      | None ->
          let color_of_the_5_cards = get_the_color_of_the_5_cards player in
          if
            color_of_the_5_cards <> ""
            && get_research_station (get_current_city player)
          then action_discover_remedy board player color_of_the_5_cards
          else if have_the_card_of_the_city (get_current_city player) player
          then action_construct_research_station board player
          else pass_turn board player)
