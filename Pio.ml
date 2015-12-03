open Pcore;;

exception Corrupted_file of string;;
  
let parse inc =
  let rec get_int () =
    try int_of_string(input_line(inc))
    with _ -> raise (Corrupted_file "get_int")
  in
  let rec get_automate (a:automaton) =
    try
      let s = input_line(inc) in
      if (s = "GenerationZero") then a
      else get_automate(s::a)
    with _ -> raise (Corrupted_file "get_automate")
  in
  let rec get_generation (g:generation) k n =
    try
      let s = input_line(inc) in
      if (String.length s = 0) then g
      else (for i=0 to n-1 do g.(k).(i) <- s.[i] done;
	    get_generation g (k+1) n)
    with
    |End_of_file -> g
    |_ -> raise (Corrupted_file "get_generation")
  in
  let n = get_int() in
  if (input_line(inc) != "Regles") then raise (Corrupted_file "get_int'")
  else
    let a = get_automate [] in
    let g = Array.make_matrix n n ' ' in
    let g = get_generation g 0 n in
    n,a,g
;;
