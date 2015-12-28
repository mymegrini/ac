type state = char;;

type generation = state array array;;

type rule = string;;
	      
type automaton = rule list;;
  
type formula = int list list;;

 
let next_generation (a:automaton) (g:generation) :generation=  
  let k = 5 (*.length (List.hd a)*) in
  let n = Array.length g in
  let s = String.make k ' ' in
  let gnext = Array.make_matrix n n ' ' in

  let get_voisinage i j =
    s.[0] <- (g.(i).((j+1)mod n));
    s.[1] <- (g.((i+1)mod n).(j));
    s.[2] <- (g.(i).((n+j-1)mod n));
    s.[3] <- (g.((n+i-1)mod n).(j));
    s.[4] <- (g.(i).(j))
  in

  for i=0 to n-1 do
    for j=0 to n-1 do
      get_voisinage i j;
      gnext.(i).(j) <- if List.mem s a then 'A' else 'D'
    done
  done;
  gnext
;;

  
let show_generation (g:generation) =
  let rec line = function
    |0 -> print_string("*\n")
    |n -> print_string("*---"); line(n-1)
  in

  print_string "\n";
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
  line n;
  print_string "\n"
;;

let stables (a:automaton) n :formula =
  let var i j = ((i+n)mod n)*n+((j+n)mod n) in
  let complement a =
    let v = String.make 1 'A' in
    let m = String.make 1 'D' in
    let rec gen_rules k = function
      |[] -> gen_rules (k-1) [[v]]
      |l when k=0 -> l
      |l -> gen_rules (k-1) List.(flatten(map (fun l -> [v::l;m::l]) l))
    in    
    List.(filter (fun c -> not(mem c a))
		 (map (fun l -> String.concat "" l) (gen_rules 5 [])))
  in
  let stable f (i,j) =

    let vos i j = function 'D' -> var i j | _ -> - var i j in
    
    List.map (fun b -> [vos i (j+1) b.[0];
			vos (i+1) j b.[1];
			vos i (j-1) b.[2];
			vos (i-1) j b.[3];
			vos i j b.[4]])
	     f
  in

  let c = complement a in
  let a = List.filter (fun b -> b.[4] = 'D') a in
  let f = List.fold_left (fun l x -> x::l) a c in
  let iter = Array.init (n*n) (fun k -> (k/n,k mod n)) in
  List.flatten (Array.fold_left (fun l x -> (stable f x)::l) [] iter);
;;

let sleep s =
  let t = Sys.time() in
  while Sys.time()<t+.s do () done
;;
