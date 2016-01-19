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
type formula = string list
type file = string
  
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

let writelist file list =
  let rec aux cou = function
    |[] -> close_out cou
    |s::t -> output_string cou (s^"\n");
	     aux cou t
  in

  aux (open_out file) list

      
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
  lines:= "States"::!lines;
  flushint lines ms;
  lines:= "Vicinity"::!lines;
  flushvicinity compass lines;
  lines:= "Rules"::!lines;
  flushrules rules lines ms vsize;
  lines:= "Generation"::!lines;
  flushgeneration universe ms lines;
  writelist file (List.rev !lines)

      
let maxstate = 1
		 
let size = 10

let vsize = 5
	     
let universe = Array.make_matrix size size 0
				 
let generation = { maxstate = maxstate; universe = universe;}
		   
let defaultcompass = [|0,1; 1,0; 0,-1; -1,0; 0,0|]
		       
let bicompass = [|0,2; 1,1; 2,0; 1,-1; 0,-2; -1,-1; -2,0; -1,1;
		  0,1; 1,0; 0,1; 1,0; 0,-1; -1,0; 0,0|]
		  
let tricompass = [|0,3; 1,2; 2,1; 3,0; 2,-1; 1,-2; 0,-3; -1,-2; -2,-1; -3,0; -2,1; -1,2;
		   0,2; 1,1; 2,0; 1,-1; 0,-2; -1,-1; -2,0; -1,1;
		   0,1; 1,0; 0,1; 1,0; 0,-1; -1,0; 0,0|]
		   
let rules = Array.init (e (maxstate+1) vsize) (fun i -> 1+(i mod (maxstate)))
		       
let automaton = { vicinity = bicompass; rules = rules;};;

flush "genx" automaton generation
