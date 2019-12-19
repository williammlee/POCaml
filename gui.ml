open Graphics
open Images
open Png
open Sdl
open Sdlmixer
open Status

type colors = Graphics.color array array

let px_size = 16
let scaling = ref 2

let on_widgets_dir = "battle_sprites/text/on_widgets/"
let on_message_dir = "battle_sprites/text/on_message/"
let on_fight_dir = "battle_sprites/text/on_fight/"

let worldgui_did_setup = ref false
let reset_worldgui = ref false
let template = ref None
let player_status = ref None

let battlegui_did_setup = ref false
let is_battling = ref false
let enemy_status = ref None

let unwrap_option = function
  | Some s -> s
  | None -> failwith "failed to unwrap"

let start_music music =
  let music = load_music music in
  play_music music

let start_sound sound =
  let sound = loadWAV sound in
  play_sound sound

(** [apply_transparency img] is an Rgb24 representation of a Png, where the
      transparent pixels are accounted for using the Graphics library unique
      transparent color. *)
let apply_transparency = function
  | Rgba32 bitmap ->
    let w = bitmap.Rgba32.width
    and h = bitmap.Rgba32.height in
    Array.init h (fun i ->
        Array.init w (fun j ->
            let {color = {r = r; g = g; b = b}; 
                 alpha = a} = Rgba32.unsafe_get bitmap j i in
            if a = 0 then Graphics.transp else rgb r g b))
  | _ -> failwith "impossible - always PNG with RGBA"

let scale_window width height scaler scaling =
  scaling := scaler !scaling;
  if !scaling < 2 then scaling := 2;
  if !scaling > 4 then scaling := 4;
  Graphics.resize_window (width * !scaling) (height * !scaling)

(** [scale_img scale img] is a scaled version of [img] where each pixel in its
    original size is repeated [scale] times in succession. *)
let scale_img scale img =
  let list_img = Array.(map Array.to_list img |> to_list) in
  let rec nappend a b n = if n > 0 then nappend (Array.append a [|b|]) b (n-1) 
    else a in
  List.fold_left (fun acc row ->
      nappend acc (List.fold_left (fun racc color -> nappend racc color scale) 
                     [||] row) scale) [||] list_img

let draw_img img x y scale is_grid =
  let g = img |> apply_transparency |> scale_img scale |> 
          Graphics.make_image in
  let coord_scaling = if is_grid then scale*px_size else scale in
  Graphics.draw_image g (x * coord_scaling) (y * coord_scaling)

let prepare_draw dir fn x y =
  let img = Png.load (dir^fn^".png") [] in
  draw_img img x y !scaling false

(* https://stackoverflow.com/questions/10068713/string-to-list-of-char *)
let explode (s : string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let prepare_draw_string dir s pos spacing =
  let (xpos,ypos) = pos in
  let (x_spacing, y_spacing) = spacing in
  (* if drawing horizontally backwards, start placing letters from the end *)
  let ordered_clst = if fst spacing < 0 then List.rev (explode s) 
    else explode s in
  (** [one_by_one pos off_pos spacing clst] draws each char in [clst] according
      to their [pos] + [off_pos], changing the offset by [spacing] per char. *)
  let rec one_by_one pos off_pos spacing = function
    | [] -> ()
    | c :: t -> 
      let x,y = !pos in
      let x_off, y_off = !off_pos in
      let x_spacing, y_spacing = !spacing in
      (match Char.escaped c with
       | " " -> off_pos := (x_off+ x_spacing, y_off)
       (* _ represents a 'new line' *)
       | "_" -> pos := (xpos,y-16);
         off_pos := (0 ,0)
       (* # represents Lv, which is 3 pixels larger in width than numbers *)
       | "#" -> prepare_draw dir "#" (x + x_off - 3) (y + y_off);
         off_pos := (x_off+ x_spacing, y_off)
       (* the next letter after l is usually closer to it by 3 pixels, but
          farther by 2 for on_widgets *)
       | "l" -> prepare_draw dir "_l" (x + x_off) (y + y_off);
         if dir = on_widgets_dir then off_pos := (x_off + x_spacing, y_off)
         else off_pos := (x_off + x_spacing - 3, y_off)
       | "/" -> prepare_draw dir "forwardslash" (x + x_off) (y + y_off);
         off_pos := (x_off+ x_spacing, y_off)
       (* these chars are low hanging *)
       | "j" | "g" | "q" | "y" | "p" as sc -> 
         if dir = on_widgets_dir then prepare_draw dir ("_" ^ sc) (x + x_off) 
             (y + y_off - 1)
         else prepare_draw dir ("_" ^ sc) (x + x_off) (y + y_off - 2);
         off_pos := (x_off + x_spacing, y_off)
       | sc -> let cased_sc = if "a" <= sc && sc <= "z" then "_" ^ sc 
                 else sc in
         prepare_draw dir cased_sc (x + x_off) (y + y_off);
         off_pos := (x_off+ x_spacing, y_off)
      );
      one_by_one pos off_pos spacing t in
  let init_pos = ref (xpos, ypos) in
  let init_off_pos = ref (0, 0) in
  let init_spacing = ref (x_spacing, y_spacing) in
  one_by_one init_pos init_off_pos init_spacing ordered_clst