open IO;;
  
type state = int;;
type generation = state array array;;
type compass = (int*int) array;;
type automaton = {
    vicinity : compass;
    rules : state array;
  }
;;
type formula = int list list;;
type file = string;;
  

exception Corrupted_file of string;;
exception System_limits of string;;
exception Limits of string;;
  
(* exponentiation *)
let e x n =
  let rec aux r x = function
  |0 -> r
  |1 -> r*x
  |n when n<0 -> 0
  |n -> if n mod 2 = 0
	then aux r (x*x) (n/2)
	else aux (r*x) (x*x) ((n-1)/2)
  in
  aux 1 x n
;;  

let parse (file:file) =
  let line = ref 1 in
  
  let fail ()=
    raise (Corrupted_file ("Line "^(string_of_int !line)))
  in

  let parseint lines =
    if lines = [] then fail()
    else
      try
	let n = int_of_string(List.hd lines) in
	incr(line);
	n,List.tl(lines)
      with
      |_-> fail()
  in
    
  let getcoordinates line =
    let comma = String.index line ',' in
    try let x,y = (String.sub line 0 (comma-1)),
		  (String.sub line (comma+1) (String.length(line)-1))
	in (int_of_string x),(int_of_string y)
    with |_ -> fail()
  in
  
  let parsevicinity lines  =
    let rec aux v = function
      |[] -> fail()
      |c::t when c="Rules" -> v,(c::t)
      |c::t -> incr(line);
	       aux ((getcoordinates c)::v) t
    in
    let v,l = aux [] lines in
    (Array.of_list(List.rev v)),l
  in

  let getstate ns = function
    |'A' when ns=2 -> 1
    |'D' when ns=2 -> 0
    |c -> let s = int_of_char c in
	  if s<ns then s else fail()
  in
  
  let parserules nv ns lines =
    let fv = float_of_int nv in
    let fs = float_of_int ns in
    let limit = float_of_int(Sys.max_array_length) in    
    if (fs**fv) >= limit
    then raise (System_limits "maximum number of rules exceeded")
    else
      let rules = Array.make (e nv ns) 0 in      
      let rec aux ns = function
	|[] -> fail()
	|[_] -> fail()
	|c::t when c="Generation" -> (c::t)
	|c::r::t -> incr(line);
		    let rule = ref 0 in
		    String.iter (fun c -> rule := (!rule)*ns+(getstate ns c)) c;
		    if ns>2
		    then (incr(line);
			  if String.length(r)>1 then fail()
			  else (try
				   rules.(!rule)<-(getstate ns r.[0]);
				   aux ns t
				 with _-> fail()
			       )
			 )
		    else (try
			     rules.(!rule)<-1;
			     aux ns t
			   with _ -> fail()
			 )				
      in
      rules,(aux ns lines)
  in

  let parsegeneration ns lines =
    if lines = [] then fail();			  
    let n = String.length(List.hd lines) in
    if n<>List.length lines then fail();    
    let generation = Array.make_matrix n n 0 in
    let rec aux i n = function
      |[] when i=n -> ()
      |s::t when i<n -> if String.length(s)<>n then fail()
			else
			  (String.iteri (fun j c -> generation.(i).(j)<-getstate ns c) s;
			   incr(line);
			   aux (i+1) n t
			  )
      |_ -> fail()
    in
    aux 0 n lines;
    generation
  in
    
  let lines = read file in
  let n,lines = parseint lines in
  if n>255 then raise (Limits "Maximum number of states exceeded");
  if List.hd(lines)<>"Vicinity" then fail();
  incr(line);
  let v,lines = parsevicinity lines in
  if List.hd(lines)<>"Rules" then fail();
  incr(line);
  let r,lines = parserules n (Array.length v) lines in
  if List.hd(lines)<>"Generation" then fail();
  incr(line);
  let (g: generation) = parsegeneration n lines in
  n,{vicinity = v; rules = r;}, g
;;







    

