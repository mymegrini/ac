open Graphics;;
open Pcore;;
open Pio;;
  
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
