open Display

(* types *)

type cubes = { blue : int; red : int; yellow : int; black : int }

type city = {
  city_id : int;
  city_name : string;
  neighbors : string list;
  color : string;
  mutable research_station : bool; (* cubes : int array; *)
  cube : cubes;
}

let convert_string_to_color (color : string) : int =
  match color with
  | "blue" -> 0
  | "red" -> 1
  | "yellow" -> 2
  | "black" -> 3
  | _ -> raise (Failure "color not found")

module CityMap = Map.Make (String)

let test_string_color (color : string) : bool =
  match color with
  | "blue" -> true
  | "red" -> true
  | "yellow" -> true
  | "black" -> true
  | _ -> false
(* getters *)

let get_cube_colors (c : city) (color : string) : int =
  match convert_string_to_color color with
  | 0 -> c.cube.blue
  | 1 -> c.cube.red
  | 2 -> c.cube.yellow
  | 3 -> c.cube.black
  | _ -> raise (Failure "color not found")

let has_cube c =
  if get_cube_colors c "blue" > 0 then Some "blue"
  else if get_cube_colors c "red" > 0 then Some "red"
  else if get_cube_colors c "yellow" > 0 then Some "yellow"
  else if get_cube_colors c "black" > 0 then Some "black"
  else None

let get_cube (c : city) = c.cube
let get_city_id c = c.city_id
let get_color c = c.color

let get_city city_name city_map =
  try CityMap.find city_name city_map
  with Not_found -> failwith "city not found"

let get_neighbors c = c.neighbors
let get_city_name c = c.city_name
let get_research_station c = c.research_station
let get_city_color c = c.color

let get_research_station_city cities : city CityMap.t =
  CityMap.filter (fun _ c -> get_research_station c) cities

(* setters *)

let set_research_station c b = c.research_station <- b

let set_cube (c : city) (color : string) (n : int) : city =
  let cube' =
    match convert_string_to_color color with
    | 0 -> { c.cube with blue = n }
    | 1 -> { c.cube with red = n }
    | 2 -> { c.cube with yellow = n }
    | 3 -> { c.cube with black = n }
    | _ -> raise (Failure "color not found")
  in
  { c with cube = cube' }

let cube_3_or_more (c : city) : bool =
  c.cube.blue >= 3 || c.cube.red >= 3 || c.cube.yellow >= 3 || c.cube.black >= 3

let get_number_cube (c : city) : int =
  c.cube.blue + c.cube.red + c.cube.yellow + c.cube.black

let add_cube (c : city) (color : string) : city =
  set_cube c color (get_cube_colors c color + 1)

let discrease_cube (c : city) (color : string) : city =
  set_cube c color (get_cube_colors c color - 1)

(*
   let edit_cube c color remed : cubes =
     match color with
     | Blue -> if remed then { c with blue = 0 } else { c with blue = c.blue - 1 }
     | Red -> if remed then { c with red = 0 } else { c with red = c.red - 1 }
     | Yellow -> if remed then { c with yellow = 0 } else { c with yellow = c.yellow - 1 }
     | Black -> if remed then { c with black = 0 } else { c with black = c.black - 1 } *)

(* modify cause a city can not have more than 3 cubes *)

(* constructors *)

let create_city id name neighbors color =
  {
    city_id = id;
    city_name = name;
    neighbors;
    color;
    research_station = false;
    cube = { blue = 0; red = 0; yellow = 0; black = 0 };
  }

(* create city *)

(* create cities *)

let rec list_of_atoms_to_str (se : Sexplib.Sexp.t) : string =
  match se with
  | Atom s -> s
  | List [] -> ""
  | List items -> items |> List.map list_of_atoms_to_str |> String.concat " "

let list_nth_of_sexp (se : Sexplib.Sexp.t) (nbr : int) : Sexplib.Sexp.t =
  match se with
  | List l -> List.nth l nbr
  | _ -> failwith "list_nth_of_sexp: not a list or index out of bounds"

let stringmap =
  "(((Atlanta)(blue))((Chicago)(blue))((San \
   Francisco)(blue))((Montreal)(blue))((Washington)(blue))((New \
   York)(blue))((London)(blue))((Madrid)(blue))((Essen)(blue))((Paris)(blue))((Milan)(blue))((St. \
   Petersburg)(blue))((Los Angeles)(yellow))((Mexico \
   City)(yellow))((Lima)(yellow))((Santiago)(yellow))((Miami)(yellow))((Bogota)(yellow))((Buenos \
   Aires)(yellow))((Sao \
   Paulo)(yellow))((Lagos)(yellow))((Kinshasa)(yellow))((Khartoum)(yellow))((Johannesburg)(yellow))((Algiers)(black))((Istanbul)(black))((Cairo)(black))((Moscow)(black))((Baghdad)(black))((Riyadh)(black))((Tehran)(black))((Karachi)(black))((Mumbai)(black))((Delhi)(black))((Kolkata)(black))((Chennai)(black))((Bangkok)(black))((Jakarta)(red))((Pekin)(red))((Shanghai)(red))((Hong \
   Kong)(red))((Ho Chi Minh \
   City)(red))((Seoul)(red))((Taipei)(red))((Manila)(red))((Sydney)(red))((Tokyo)(red))((Osaka)(red)))"

let stringconnexions =
  "(((Algiers)((Madrid)(Paris)(Istanbul)(Cairo)))((Atlanta)((Chicago)(Washington)(Miami)))((Baghdad)((Istanbul)(Tehran)(Karachi)(Riyadh)(Cairo)))((Bangkok)((Kolkata)(Hong \
   Kong)(Ho Chi Minh \
   City)(Jakarta)(Chennai)))((Pekin)((Seoul)(Shanghai)))((Bogota)((Miami)(Sao \
   Paulo)(Buenos Aires)(Lima)(Mexico City)))((Buenos Aires)((Bogota)(Sao \
   Paulo)))((Cairo)((Istanbul)(Baghdad)(Riyadh)(Khartoum)(Algiers)))((Chennai)((Delhi)(Kolkata)(Bangkok)(Jakarta)(Mumbai)))((Chicago)((Montreal)(Atlanta)(Mexico \
   City)(Los Angeles)(San \
   Francisco)))((Delhi)((Kolkata)(Chennai)(Mumbai)(Karachi)(Tehran)))((Essen)((St. \
   Petersburg)(Milan)(Paris)(London)))((Ho Chi Minh City)((Hong \
   Kong)(Manila)(Jakarta)(Bangkok)))((Hong Kong)((Shanghai)(Taipei)(Manila)(Ho \
   Chi Minh City)(Bangkok)(Kolkata)))((Istanbul)((St. \
   Petersburg)(Moscow)(Baghdad)(Cairo)(Algiers)(Milan)))((Jakarta)((Bangkok)(Ho \
   Chi Minh \
   City)(Sydney)(Chennai)))((Johannesburg)((Khartoum)(Kinshasa)))((Karachi)((Tehran)(Delhi)(Mumbai)(Riyadh)(Baghdad)))((Khartoum)((Cairo)(Johannesburg)(Kinshasa)(Lagos)))((Kinshasa)((Khartoum)(Johannesburg)(Lagos)))((Kolkata)((Hong \
   Kong)(Bangkok)(Chennai)(Delhi)))((Lagos)((Khartoum)(Kinshasa)(Sao \
   Paulo)))((Lima)((Bogota)(Santiago)(Mexico \
   City)))((London)((Essen)(Paris)(Madrid)(New York)))((Los Angeles)((San \
   Francisco)(Chicago)(Mexico City)(Sydney)))((Madrid)((New \
   York)(London)(Paris)(Algiers)(Sao Paulo)))((Manila)((Taipei)(San \
   Francisco)(Sydney)(Ho Chi Minh City)(Hong Kong)))((Mexico \
   City)((Chicago)(Miami)(Bogota)(Lima)(Los \
   Angeles)))((Miami)((Washington)(Bogota)(Mexico \
   City)(Atlanta)))((Milan)((Istanbul)(Paris)(Essen)))((Montreal)((New \
   York)(Washington)(Chicago)))((Moscow)((Tehran)(Istanbul)(St. \
   Petersburg)))((Mumbai)((Karachi)(Delhi)(Chennai)))((New \
   York)((London)(Madrid)(Washington)(Montreal)))((Osaka)((Tokyo)(Taipei)))((Paris)((Essen)(Milan)(Algiers)(Madrid)(London)))((Riyadh)((Baghdad)(Karachi)(Cairo)))((San \
   Francisco)((Chicago)(Los Angeles)(Manila)(Tokyo)))((Santiago)((Lima)))((Sao \
   Paulo)((Madrid)(Lagos)(Buenos \
   Aires)(Bogota)))((Seoul)((Tokyo)(Shanghai)(Pekin)))((Shanghai)((Pekin)(Seoul)(Tokyo)(Taipei)(Hong \
   Kong)))((St. Petersburg)((Moscow)(Istanbul)(Essen)))((Sydney)((Manila)(Los \
   Angeles)(Jakarta)))((Taipei)((Osaka)(Manila)(Hong \
   Kong)(Shanghai)))((Tehran)((Delhi)(Karachi)(Baghdad)(Moscow)))((Tokyo)((San \
   Francisco)(Osaka)(Shanghai)(Seoul)))((Washington)((New \
   York)(Miami)(Atlanta)(Montreal))))"

let create_map () =
  try
    let rec create_city_aux acc map_city sexpstringmap =
      if acc = 48 then map_city
      else
        let city_info = list_nth_of_sexp sexpstringmap acc in
        let city_name, city_color =
          match city_info with
          | List [ name; color ] ->
              (list_of_atoms_to_str name, list_of_atoms_to_str color)
          | _ -> failwith "stringmap is not well formated"
        in
        let city =
          let city' = create_city acc city_name [] city_color in
          if city_name = "Atlanta" then { city' with research_station = true }
          else city'
        in
        create_city_aux (acc + 1)
          (CityMap.add city_name city map_city)
          sexpstringmap
    in
    let map_city =
      create_city_aux 0 CityMap.empty (Sexplib.Sexp.of_string stringmap)
    in
    try
      let rec create_connections_aux acc city_map sexpstringconnexions =
        if acc = 48 then city_map
        else
          let city_info = list_nth_of_sexp sexpstringconnexions acc in
          let city_name =
            match city_info with
            | List [ name; _ ] -> list_of_atoms_to_str name
            | _ -> failwith "stringconnexions is not well formated"
          in
          let city_neighbors =
            match city_info with
            | List [ _; neighbors ] -> (
                match neighbors with
                | List l -> l |> List.map list_of_atoms_to_str
                | _ -> failwith "stringconnexions is not well formated")
            | _ -> failwith "stringconnexions is not well formated"
          in
          let city = get_city city_name city_map in
          let city = { city with neighbors = city_neighbors } in
          create_connections_aux (acc + 1)
            (CityMap.update city_name (fun _ -> Some city) city_map)
            sexpstringconnexions
      in
      create_connections_aux 0 map_city
        (Sexplib.Sexp.of_string stringconnexions)
    with e ->
      (* some unexpected exception occurs *)
      (* emergency closing *)
      raise e
  with e ->
    (* some unexpected exception occurs *)
    (* emergency closing *)
    raise e

(* Booleans *)
let city_equals city1 city2 = city1.city_id = city2.city_id

(* print *)

let ppcity fmt city =
  match city.color with
  | "blue" -> Format.fprintf fmt "@{<fg_blue>%s@}" city.city_name
  | "yellow" -> Format.fprintf fmt "@{<fg_yellow>%s@}" city.city_name
  | "red" -> Format.fprintf fmt "@{<fg_red>%s@}" city.city_name
  | "black" -> Format.fprintf fmt "@{<fg_black>%s@}" city.city_name
  | _ -> Format.fprintf fmt "%s " city.city_name

let ppcubes fmt city =
  Format.fprintf fmt
    "@{<bg_blue>%d@} @{<bg_red>%d@} @{<bg_yellow>%d@} @{<bg_black>%d@}"
    city.cube.blue city.cube.red city.cube.yellow city.cube.black

let ppneighbors fmt ((city, city_list) : city * city CityMap.t) =
  let n = get_neighbors city in
  List.iter (fun c -> Format.fprintf fmt "%a@ " ppcity (get_city c city_list)) n

let ppcity_full fmt (city, map) =
  Format.fprintf fmt "%a@." ppcity city;
  Format.fprintf fmt "neighbors: %a@." ppneighbors (city, map);
  Format.fprintf fmt "research station: %b@." city.research_station;
  Format.fprintf fmt "cubes: %a@." ppcubes city
(* add_ansi_marking Format.std_formatter;
   CityMap.iter (fun _ city -> Format.printf "%a@ " ppcity city) city_list *)

let ppresearch_stations fmt ((city, city_list) : city * city CityMap.t) =
  CityMap.iter
    (fun _ c ->
      if get_research_station c && c <> city then
        Format.fprintf fmt "%a@ " ppcity c
      else ())
    city_list

let print_map dict =
  add_ansi_marking Format.std_formatter;
  CityMap.iter (fun _ city -> Format.printf "%a@ " ppcity city) dict

let set_research_station_to_city (map : city CityMap.t) (city_name : string) :
    city CityMap.t =
  CityMap.map
    (fun x ->
      if get_city_name x = city_name then { x with research_station = true }
      else x)
    map

let city_in_map (map : city CityMap.t) (city : string) =
  CityMap.exists (fun _ c -> get_city_name c = city) map
