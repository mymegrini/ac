open Pcore;;
open Pio;;
open Pgraphics;;

let main () =
  let gz = ref "" in
  
  if Array.length Sys.argv > 1 && Sys.argv.(1) <> "-s"
  then gz := Sys.argv.(1)
  else gz := "genz";

  let cin = open_in !gz in
  let n,a,g = parse cin in

  show_generation g;
  print_string "\nshow next generation? (Y/n):";

  let rec aux s =
    if String.length s != 1
    then (match s.[0] with
	  |'y'|'Y' -> print_string "\nshow next generation? (Y/n):";
		      aux(read_line())
	  |'n'|'N' -> ()
	  |_ -> print_string "\nshow next generation? (Y/n):"
	 )
    else (print_string "\nshow next generation? (Y/n):";
	  aux(read_line())
	 )
  in
	       
  aux(read_line())
;;

main();;
