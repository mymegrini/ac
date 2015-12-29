let read file =
  let rec aux cin l =
    match (try Some(input_line cin) with End_of_file -> None) with
    |None -> l
    |Some s -> aux cin (s::l)
  in
  
  let cin = open_in file in
  let list = List.rev (aux []) in
  close_in cin;  
  list
;;
  
let write file list =
  let rec aux cou = function
    |[] -> close_out cou
    |s::t -> output_string(s^"\n");
	     aux cou t
  in

  aux (open_out file) list
;;

