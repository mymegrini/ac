open Pcore;;
open Pio;;
open Pgraphics;;

let main () =
  let gz = ref "genz" in
    
  if Array.length Sys.argv > 1
  then
    match Sys.argv.(1) with
      |"-s" -> if Array.length Sys.argv < 4
               then print_string "Insufficient arguments.\n"
               (*else genstables int_of_string(Sys.argv.(2))
                               int_of_string(Sys.argv.(3))*)
      |"-g" -> if Array.length Sys.argv < 4
               then print_string "Insufficient arguments.\n"
               (*else gengraphics  int_of_string(Sys.argv.(2))
                                 int_of_string(Sys.argv.(3))*)
      | s -> (
          gz:= s;
          let cin = open_in !gz in
          let n,a,g = parse cin in
          close_in cin;
          
          let gref = ref g in
          show_generation gref;
      
          while answer "Show next generation? [Y/n]" do
            gref:= next_generation a (!gref);
            show_generation !gref
          done;
  
          if answer "Save current generation? [Y/n]"
          then (
            let cout = open_out !gz in
              flush cout n a g;
              close_out cout
          )
        )
;;

main();;
