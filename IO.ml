(* read a file *)
let readlist file =
  let rec aux cin l =
    match (try Some(input_line cin) with End_of_file -> None) with
    |None -> l
    |Some s -> aux cin (s::l)
  in
  
  let cin = open_in file in
  let list = List.rev (aux cin []) in
  close_in cin;  
  list

(* write a file *)
let writelist file list =
  let rec aux cou = function
    |[] -> close_out cou
    |s::t -> output_string cou (s^"\n");
	     aux cou t
  in

  aux (open_out file) list

(* initialize minisat input file *)
      
let header = ref ""		 
let initsat f n =      
  header:= "p cnf "^string_of_int(n*n)^" "^string_of_int(List.length f);
  writelist "dimacs" (!header::f)
  
(* read minisat output and update formula *)
let outputsat () = 
  let neg var =
    if var.[0] = '-'
    then String.sub var 1 (String.length(var) -1)
    else "-"^var
  in
  
  let string_to_list clause =
    let index = ref 0 in
    let list = ref [] in
    String.iteri
      (fun i c ->
	if c = ' '
	then (list:= (String.sub clause (!index) (i- !index))::!list; index:=i+1)
      ) clause;
    !list
  in

  let negclause list =
    String.concat " " (List.fold_left (fun acc var -> (neg var)::acc) ["0"] list)
  in

  let update_header () =
    let n = String.length !header in
    let index = String.rindex !header ' ' in
    let nc = int_of_string(String.sub (!header) (index+1) (n-index-1)) in
    header:= (String.sub (!header) 0 (index+1))^string_of_int(nc+1);
  in
  
  let append clause =
    let cout = open_out_gen [Open_append] 777 "dimacs" in
    let seek_end = out_channel_length(cout) -1 in
    seek_out cout seek_end;
    output_string cout (clause^"\n");
    close_out cout;
    update_header();
    let cout = open_out_gen [Open_wronly] 777 "dimacs" in
    seek_out cout 0;
    output_string cout (!header^"\n");
    close_out cout
  in
  
  match readlist "valuation" with
  |"SAT"::clause::t -> let clause = string_to_list clause in
		       append (negclause clause);
		       List.map int_of_string clause
  |_ -> []
