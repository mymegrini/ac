#load "graphics.cma";;
open Graphics;;
open Pcore;;
open Pio;;

type box = int array;;
type window = box list;;

let sx = 800 and sy = 600;;
  
let show_generation (g:generation) =
  let rec line = function
    |0 -> print_string("*\n")
    |n -> print_string("*---"); line(n-1)
  in

  let n = Array.length(g) in
  for i=0 to n-1 do
    line n;
    for j=0 to n-1 do
      print_string "| ";
      print_char(g.(i).(j));
      print_char ' '
    done;
    print_string "|\n";
  done;
  line n
;;

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


  
  
  
