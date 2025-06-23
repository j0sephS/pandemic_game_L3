open Format

type style =
  | Normal
  | Italic
  | Italic_off
  | FG_Black
  | FG_Red
  | FG_Green
  | FG_Yellow
  | FG_Blue
  | FG_Magenta
  | FG_Cyan
  | FG_Default
  | BG_Black
  | BG_Red
  | BG_Green
  | BG_Yellow
  | BG_Blue
  | BG_Magenta
  | BG_Cyan
  | BG_White
  | BG_Default

let to_ansi_value = function
  | Normal -> "0"
  | Italic -> "3"
  | Italic_off -> "23"
  | FG_Black -> "30"
  | FG_Red -> "31"
  | FG_Green -> "32"
  | FG_Yellow -> "33"
  | FG_Blue -> "34"
  | FG_Magenta -> "35"
  | FG_Cyan -> "36"
  | FG_Default -> "39"
  | BG_Black -> "40"
  | BG_Red -> "41"
  | BG_Green -> "42"
  | BG_Yellow -> "43"
  | BG_Blue -> "44"
  | BG_Magenta -> "45"
  | BG_Cyan -> "46"
  | BG_White -> "47"
  | BG_Default -> "49"

let close_tag = function
  | Italic -> Italic_off
  | FG_Black | FG_Red | FG_Green | FG_Yellow | FG_Blue | FG_Magenta | FG_Cyan
  | FG_Default ->
      FG_Default
  | BG_Black | BG_Red | BG_Green | BG_Yellow | BG_Blue | BG_Magenta | BG_Cyan
  | BG_White | BG_Default ->
      BG_Default
  | _ -> Normal

let style_of_tag = function
  | String_tag s -> (
      match s with
      | "n" -> Normal
      | "italic" -> Italic
      | "/italic" -> Italic_off
      | "fg_black" -> FG_Black
      | "fg_red" -> FG_Red
      | "fg_green" -> FG_Green
      | "fg_yellow" -> FG_Yellow
      | "fg_blue" -> FG_Blue
      | "fg_magenta" -> FG_Magenta
      | "fg_cyan" -> FG_Cyan
      | "fg_default" -> FG_Default
      | "bg_black" -> BG_Black
      | "bg_red" -> BG_Red
      | "bg_green" -> BG_Green
      | "bg_yellow" -> BG_Yellow
      | "bg_blue" -> BG_Blue
      | "bg_magenta" -> BG_Magenta
      | "bg_cyan" -> BG_Cyan
      | "bg_white" -> BG_White
      | "bg_default" -> BG_Default
      | _ -> raise Not_found)
  | _ -> raise Not_found

let ansi_tag = Printf.sprintf "\x1B[%sm"

let start_mark_ansi_stag t =
  try ansi_tag (to_ansi_value (style_of_tag t)) with Not_found -> ""

let stop_mark_ansi_stag t =
  try ansi_tag (to_ansi_value (close_tag (style_of_tag t)))
  with Not_found -> ""

let add_ansi_marking formatter =
  let open Format in
  pp_set_mark_tags formatter true;
  let old_fs = pp_get_formatter_stag_functions formatter () in
  pp_set_formatter_stag_functions formatter
    {
      old_fs with
      mark_open_stag = start_mark_ansi_stag;
      mark_close_stag = stop_mark_ansi_stag;
    }
