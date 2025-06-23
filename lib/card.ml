open City

(* type *)
type typeCards = Infection | Player | Event | Epidemic

type card = {
  card_city_name : string;
  card_color : string;
  card_type : typeCards;
}

(* constructor *)
let name_valid name =
  let map = create_map () in
  city_in_map map name || name = "Forecast" || name = "Airlift"
  || name = "Government Grant"
  || name = "Resilient Population"
  || name = "One Quiet Night"

let init_card name color cType =
  if cType = "Epidemic" && name = "" && color = "" then
    { card_city_name = name; card_color = color; card_type = Epidemic }
  else if name_valid name then
    if cType = "Event" && color = "" then
      { card_city_name = name; card_color = color; card_type = Event }
    else if test_string_color color then
      {
        card_city_name = name;
        card_color = color;
        card_type =
          (match cType with
          | "Infection" -> Infection
          | "Player" -> Player
          | "Event" -> Event
          | "Epidemic" -> Epidemic
          | _ -> failwith "Invalid card type");
      }
    else failwith "Invalid color"
  else failwith "Invalid arguments"

let get_type_card (c : card) : string =
  match c.card_type with
  | Player -> "Player"
  | Infection -> "Infection"
  | Event -> "Event"
  | Epidemic -> "Epidemic"

let create_cards (cities : (string * city) list) (type_card : typeCards) :
    card list =
  List.map
    (fun (name, city) ->
      {
        card_city_name = name;
        card_color = get_city_color city;
        card_type = type_card;
      })
    cities

let create_infection_cards (cities : (string * city) list) : card list =
  create_cards cities Infection

let card_is_event c = c.card_type = Event

let create_event_cards (event_name : string) : card =
  match event_name with
  | "Forecast" -> init_card event_name "" "Event"
  | "Airlift" -> init_card event_name "" "Event"
  | "Government Grant" -> init_card event_name "" "Event"
  | "Resilient Population" -> init_card event_name "" "Event"
  | "One Quiet Night" -> init_card event_name "" "Event"
  | _ -> raise (Failure "wrong event name")

let shuffle_list (l : card list) =
  Random.self_init ();
  let rec shuffle_aux l =
    match l with
    | [] -> []
    | [ single ] -> [ single ]
    | list ->
        let before, after = List.partition (fun _ -> Random.bool ()) list in
        List.rev_append (shuffle_aux before) (shuffle_aux after)
  in
  shuffle_aux l

let split_list list number_of_split : 'a list list =
  if number_of_split <= 0 then failwith "Invalid argument"
  else
    let rec split_list_aux list number_of_split listoflist =
      if number_of_split = 1 then list :: listoflist
      else
        let rec take list n acc =
          if n = 0 then (acc, list)
          else
            match list with
            | [] -> failwith "Invalid argument"
            | h :: t -> take t (n - 1) (h :: acc)
        in
        let l1, l2 = take list (List.length list / number_of_split) [] in
        split_list_aux l2 (number_of_split - 1) (l1 :: listoflist)
    in
    split_list_aux list number_of_split []

let create_epidemic_and_event_cards (player_list : card list) difficulty_level =
  let difficulty =
    match difficulty_level with
    | "Easy" -> 4
    | "Standard" -> 5
    | "Heroic" -> 6
    | _ -> failwith "Invalid difficulty level"
  in

  let event_cards =
    [
      create_event_cards "Forecast";
      create_event_cards "Airlift";
      create_event_cards "Government Grant";
      create_event_cards "Resilient Population";
      create_event_cards "One Quiet Night";
    ]
  in
  let new_player_list = shuffle_list (player_list @ event_cards) in
  let new_player_list_list = split_list new_player_list difficulty in
  let rec create_epidemic_cards (player_list_list : card list list) =
    match player_list_list with
    | [] -> []
    | h :: t ->
        let epidemic_card = init_card "" "" "Epidemic" in
        let new_player_list = shuffle_list (epidemic_card :: h) in
        new_player_list @ create_epidemic_cards t
  in
  create_epidemic_cards new_player_list_list

let create_players_cards (cities : (string * city) list) : card list =
  create_cards cities Player

(* print *)
let ppcard (fmt : Format.formatter) (c : card) : unit =
  if c.card_type = Infection || c.card_type = Player || c.card_type = Event then
    let _ =
      match c.card_type with
      (* let _ because without it, if the card type is Infection, it will not print the card city name *)
      | Infection ->
          Format.fprintf fmt "@[<h 2>@{<bg_green>Infection Card@ :@}@ @]"
      | Player -> Format.fprintf fmt "@[<h 2>@{<bg_red>Player Card@ :@}@ @]"
      | Event -> Format.fprintf fmt "@[<h 2>@{<bg_blue>Event Card@ :@}@ @]"
      | _ -> failwith "Invalid card type"
    in
    match c.card_color with
    | "blue" -> Format.fprintf fmt "@{<bg_blue>%s@}@." c.card_city_name
    | "yellow" -> Format.fprintf fmt "@{<bg_yellow>%s@}@." c.card_city_name
    | "black" -> Format.fprintf fmt "@{<bg_black>%s@}@." c.card_city_name
    | "red" -> Format.fprintf fmt "@{<bg_red>%s@}@." c.card_city_name
    | "" ->
        Format.fprintf fmt "@{<fg_green>%s@}@."
          c.card_city_name (* event card *)
    | _ -> failwith "Invalid color"
  else failwith "Invalid card type"

let print_card_list (cards : card list) : unit =
  let rec print_card_list_helper cards =
    match cards with
    | [] -> Format.printf "@."
    | h :: t ->
        Format.printf "%a" ppcard h;
        print_card_list_helper t
  in
  print_card_list_helper cards

(* getters *)

let get_card_city_name c = c.card_city_name
let get_card_color c = c.card_color
let get_city_name_of_card (c : card) : string = c.card_city_name

let get_city_of_card (c : card) (map : city CityMap.t) : city =
  get_city c.card_city_name map

(* Booleans *)
let card_is_player c = c.card_type = Player
let card_is_epidemic c = c.card_type = Epidemic

let get_card_event_name (c : card) : string =
  match c.card_type with
  | Event -> c.card_city_name
  | _ ->
      print_endline "Pas event";
      ""
(* | _ -> failwith "Not an event card" *)

let card_equals (c1 : card) (c2 : card) : bool =
  if c1.card_type = Event then get_card_event_name c1 = get_card_event_name c2
  else c1.card_type = c2.card_type && c1.card_city_name = c2.card_city_name

let card_equals_string (c : card) (s : string) : bool = c.card_city_name = s

let rec find_card (card_list : card list) (card_name : string) : card option =
  match card_list with
  | [] -> None
  | h :: q ->
      if h.card_city_name = card_name then Some h else find_card q card_name
