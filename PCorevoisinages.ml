type state = char;;

type generation = state array array;;

type rule = string;;

type automaton = {voisinage : (int*int) array;mutable regles :rule list};;

type formula = int list list;;

(* mes fonctions auxiliaires *)
let range n =
  let rec aux n acc = match n with
      0 -> 0::acc
     |_ -> aux (n-1) (n::acc) in
  aux (n-1) [];;
let ki (*keep inside*) k n = match (k mod n) with
    t when t<0 -> t+n
   |_ -> k mod n;;

(* le reste *)
let next_generation a (g:generation) :generation=
  let n = Array.length g in
  let k = Array.length (a.voisinage) in
  let s = String.make k ' ' in
  let gnext = Array.make_matrix n n ' ' in

  let get_voisinage i j s v=
    let f t c = (g.(ki (i+(fst v.(t))) n).(ki (j+(snd v.(t))) n))
    in String.mapi f s  
  in
  for i=0 to n-1 do
    for j=0 to n-1 do
      let s = get_voisinage i j s (a.voisinage) in
      gnext.(i).(j) <- if List.mem s (a.regles) then 'A' else 'D';
    done
  done;
  gnext
;;

let stables (a:automaton) n :formula =
  let var i j = ((i+n)mod n)*n+((j+n)mod n) in
  let complement a k=
    let v = String.make 1 'A' in
    let m = String.make 1 'D' in
    let rec gen_rules k = function
      |[] -> gen_rules (k-1) [[v]]
      |l when k=0 -> l
      |l -> gen_rules (k-1) List.(flatten(map (fun l -> [v::l;m::l]) l))
    in
    List.(filter (fun c -> not(mem c a))
                 (map (fun l -> String.concat "" l) (gen_rules k [])))
  in
  let stable f (i,j) v=

    let vos i j = function 'D' -> var i j | _ -> - var i j in
    let l = range (Array.length v) in
    List.map (fun b -> List.map (fun t -> vos (ki (i+(fst v.(t))) n ) (ki (j+(snd v.(t))) n) b.\
[t]) l) f
  in

  let c = complement a.regles (Array.length a.voisinage) in
  let b = List.filter (fun b -> b.[4] = 'D') a.regles in
  let f = List.fold_left (fun l x -> x::l) b c in
  let iter = Array.init (n*n) (fun k -> (k/n,k mod n)) in
  List.flatten (Array.fold_left (fun l x -> (stable f x)::l) [] iter);
;;

