open IO
  
type state = int
type generation = {
    maxstate : state;
    universe : state array array
  }
type compass = (int*int) array
type automaton = {
    vicinity : compass;
    rules : state array;
  }
type formula = int list list
type file = string
  

exception Corrupted_file of string
exception System_limits of string
exception Limits of string
exception Badcell of (int*int)
			   
let defaultcompass = [|0,1; 1,0; 0,-1; -1,0; 0,0|]
let bicompass = [|0,2; 1,1; 2,0; 1,-1; 0,-2; -1,-1; -2,0; -1,1;
		  0,1; 1,0; 0,1; 1,0; 0,-1; -1,0; 0,0|]
let tricompass = [|0,3; 1,2; 2,1; 3,0; 2,-1; 1,-2; 0,-3; -1,-2; -2,-1; -3,0; -2,1; -1,2;
		   0,2; 1,1; 2,0; 1,-1; 0,-2; -1,-1; -2,0; -1,1;
		   0,1; 1,0; 0,1; 1,0; 0,-1; -1,0; 0,0|]
let maxstate = 255
    
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

(* pausing execution *)
let sleep s =
  
  let t = Sys.time() in
  while Sys.time()<t+.s do () done
      
(* parsing a file *)
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
    try let comma = String.index line ',' in
	let x,y = (String.sub line 0 comma),
		  (String.sub line (comma+1) (String.length(line)-comma-1))
	in (int_of_string x),(int_of_string y)
    with |_ -> print_string(line); fail()
  in
  
  let parsevicinity lines  =
    let rec aux v = function
      |[] -> fail()
      |c::t when c="Rules" -> v,(c::t)
      |c::t -> let c' = getcoordinates c in
	       incr(line);
	       aux (c'::v)  t
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
  
  let parserules ns nv lines =
    let fv = float_of_int nv in
    let fs = float_of_int ns in
    let limit = float_of_int(Sys.max_array_length) in    
    if (fs**fv) >= limit
    then raise (System_limits "maximum number of rules exceeded")
    else
      let rules = Array.make (e ns nv) 0 in      
      let rec aux ns = function
	|[] -> fail()
	|[_] -> fail()
	|l::t when l="Generation" -> (l::t)
	|l::r::t -> let rule = ref 0 in
		    String.iter (fun c -> rule := ns*(!rule)+(getstate ns c)) l;
		    if ns>2
		    then (incr(line);
			  if String.length(r)>1 then fail()
			  else (try
				   rules.(!rule)<-(getstate ns r.[0]);
				   incr(line);
				   aux ns t
				 with _-> fail()
			       )
			 )
		    else (try
			     rules.(!rule)<-1;
			     incr(line);
			     aux ns (r::t)
			   with _ -> fail()
			 )
      in
      rules,(aux ns lines)
  in

  let parsegeneration ns n lines =
    if lines = [] then fail();  
    let u = Array.make_matrix n n 0 in
    let rec aux i n = function
      |[] when i=n -> ()
      |s::t when i<n -> if String.length(s)<>n then fail()
			else
			  (String.iteri (fun j c -> u.(i).(j)<-getstate ns c) s;
			   incr(line);
			   aux (i+1) n t
			  )
      |_ -> fail()
    in
    aux 0 n lines;
    {maxstate = ns-1; universe = u}
  in
  
  let lines = ref(read file) in
  let n,l = parseint !lines in
  lines := l;
  let ms = ref 2 in
  if List.hd(!lines)="States"
  then (
    incr(line);
    let s,l = parseint (List.tl !lines) in
    ms := s;
    lines := l
  );
  if !ms>maxstate then raise (Limits "Maximum number of states exceeded");
  let compass = ref defaultcompass in
  if List.hd(!lines)="Vicinity"
  then (
    incr(line);
    let v,l = parsevicinity (List.tl !lines) in
    compass := v;
    lines := l
  );
  if List.hd(!lines)<>"Rules" then fail();
  incr(line);
  let r,l = parserules (!ms) (Array.length !compass) (List.tl !lines) in
  lines := l;
  if List.hd(!lines)<>"Generation" then (print_string(List.hd(!lines));fail());
  incr(line);
  let (g: generation) = parsegeneration (!ms) n (List.tl !lines) in
  n,{vicinity = !compass; rules = r;}, g

(* printing a generation in standard output *)
let show_generation (g:generation) =
  
  let rec line = function
    |0 -> print_string("*\n")
    |n -> print_string("*---"); line(n-1)
  in
  let getchar ms = function
    |0 when ms=1 -> 'D'
    |1 when ms=1 -> 'A'
    |i -> char_of_int i
  in  
  let ms,u = g.maxstate,g.universe in
  print_string "\n";
  let n = Array.length(u) in
  for i=0 to n-1 do
    line n;
    for j=0 to n-1 do
      print_string "| ";
      (try print_char(getchar ms (u.(i).(j))) with _ -> raise (Badcell (i,j)));
      print_char ' '
    done;
    print_string "|\n";
  done;
  line n;
  print_string "\n"

(* generating next generation *)
let seeku universe i j =
  
  let n = Array.length universe in
  universe.((i+n) mod n).((j+n) mod n)
    
let seekv compass ns rule i j =
  
  let index =
    let k = ref(-1) in
    Array.iteri (fun n (a,b) -> if (!k = -1) && (a,b)=(i,j) then k := n+1) compass;
    Array.length compass - !k
  in
  (rule/(e ns index)) mod ns
		       
let vicinity compass ns seek i j =
  
  let rule = ref 0 in
  Array.iteri (fun n (a,b) -> rule := ns*(!rule)+(seek (i+a) (j+b))) compass;
  !rule
   
let next_generation (a:automaton) (g:generation) :generation=
  
  let usize = Array.length g.universe in
  let ns = g.maxstate+1 in
  let u = g.universe in
  let rules = a.rules in
  let compass = a.vicinity in
  let universe = Array.make_matrix usize usize 0 in
  for i = 0 to usize-1 do
    for j =0 to usize-1 do
      universe.(i).(j)<- rules.(vicinity compass ns (seeku u) i j)
    done
  done;
  {maxstate = g.maxstate; universe = universe}

(* generating stability formula *)
let stables (a:automaton) usize :formula =
  let var i j =
    ((i+usize)mod usize)*usize+((j+usize)mod usize)+1
  in  
  
  let neg i j = function
    |0 -> var i j
    |1 -> - var i j
    |_ -> raise(Badcell(i,j))
  in
  
  let clause rule compass i j =
    let vsize = ref(Array.length(compass)) in
    Array.fold_left
      (fun l (a,b) -> decr(vsize);
		      (neg(i+a)(j+b)((rule/(e 2 !vsize)) mod 2))::l
      )
      []
      compass      
  in
  
  let stable compass (i,j) =
    let l = ref [] in
    Array.iteri
      (fun rule state -> if (rule mod 2)<>state
			 then l:=(clause rule compass i j)::(!l)
      )
      (a.rules);
    !l
  in   
  
  let compass = a.vicinity in
  let iter = Array.init (usize*usize) (fun k -> (k/usize,k mod usize)) in
  List.flatten (Array.fold_left (fun l x -> (stable compass x)::l) [] iter)
	       

(* saving into file *)
let flush (file:file) (a:automaton) (g:generation) =
  
  let flushint lines n =
    lines := (string_of_int n)::!lines
  in

  let flushvicinity compass lines =
    Array.iter
      (fun (i,j) -> lines:= ((string_of_int i)^","^(string_of_int j))::!lines)
      compass
  in
  
  let getchar ms = function
    |0 when ms=1 -> 'D'
    |1 when ms=1 -> 'A'
    |i -> char_of_int i
  in
  
  let rulestring rule state ms vsize =
    let digit i =
      (rule/(e (ms+1) (vsize-i-1))) mod (ms+1)
    in
    
    (String.init vsize (fun i -> getchar ms (digit i)))
    ^(if ms>1 then "\n"^string_of_int(state) else "")
  in

  let flushrules rules lines ms vsize =
    Array.iteri
      (fun rule state ->
	if state>0 then lines:= (rulestring rule state ms vsize)::!lines)
      rules
  in

  let getline ms line =
    let usize = Array.length line in
    String.init usize (fun i -> getchar ms (line.(i)))
  in
  
  let flushgeneration universe ms lines =
    Array.iter(fun line -> lines:= (getline ms line)::!lines) universe
  in  

  let lines = ref [] in
  let n = Array.length(g.universe) in
  let compass = a.vicinity in  
  let ms = g.maxstate in
  let rules = a.rules in
  let vsize = Array.length(a.vicinity) in
  let universe = g.universe in  
  flushint lines n;
  lines:= "Vicinity"::!lines;
  flushvicinity compass lines;
  lines:= "Rules"::!lines;
  flushrules rules lines ms vsize;
  lines:= "Generation"::!lines;
  flushgeneration universe ms lines;
  write file (List.rev !lines)
