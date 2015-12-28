#load "graphics.cma";;
open Graphics;;
open Pcore;;
open Pio;;

type box = int array;;
type window = box list;;

let sx = 800 and sy = 600;;

let gstart () =
  open_graph(" "^string_of_int(sx)^"x"^string_of_int(sy));
  set_window_title "Celular Automaton";
  set_color black;
  fill_rect 0 0 sx sy;;

let gstop () =
  close_graph();;

let rec gupdate () =
  resize_window sx sy;
  set_color black;
  fill_rect 0 0 sx sy;;


  
  
  
