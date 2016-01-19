open Libcelaut
open Graphics
       
let sx = ref 0
let sy = ref 0
let nbstate = ref 1
let universe = ref (Array.make_matrix 10 10 0)
let bsize = 20
let border = 50
let pause = 1.
	      	       		   
let sizestring x y =
  " "^string_of_int(x)^"x"^string_of_int(y)

let getcolor (state:state) =
  rgb 0 ((255*state)/ !nbstate) 0
						       
let initscreen () =
  sx:= 2*border;
  sy:= 2*border;
  open_graph(sizestring !sx !sy);
  set_window_title "ca"

let drawbox i j state =
  set_color (getcolor state);
  fill_rect (border+bsize*j) (border+bsize*i) bsize bsize;
  set_color green;
  draw_rect (border+bsize*j) (border+bsize*i) bsize bsize
  
let update () =
  let n = Array.length !universe in
  sx:= 2*border+n*bsize;
  sy:= 2*border+n*bsize;
  resize_window !sx !sy;
  set_color black;
  clear_graph();
  fill_rect 0 0 !sx !sy;
  for i=0 to n-1 do
    for j=0 to n-1 do
      drawbox (n-1-i) j (!universe.(i).(j))
    done;
  done

let sleep s =
  
  let t = Sys.time() in
  while Sys.time()<t+.s do () done
    
let rec showgrowth a g =
  if (g.universe <> !universe)
  then (universe:= g.universe;
	nbstate:= g.maxstate+1;
	update();
	sleep(pause);
	update();
	showgrowth a (next_generation a g)
       )
	   
let rec showstable a n =
  nbstate:= 2;
  match minisat a n with
  |None -> ()
  |Some(g) -> universe:= g.universe;
	      update();
	      sleep(pause);
	      showstable a n

let incrstate (state:state) :state =
  (state+1) mod (!nbstate)
  
let getinput min max =
  let n = (max-border)/bsize in
  let etat = wait_next_event[Button_down] in
  let y = etat.mouse_x in
  let x = etat.mouse_y in
  if x>=min && x<max && y>=min && y<max
  then let i = (x-border)/bsize and j = (y-border)/bsize in
       !universe.(n-1-i).(j)<-incrstate(!universe.(n-1-i).(j))

let rec edit file a g =
  universe:= g.universe;
  nbstate:= g.maxstate+1;
  update();
  let n = Array.length !universe in
  getinput border (border+bsize*n);
  update();
  let g' = {maxstate= !nbstate-1; universe= !universe;} in
  flush file a g';
  edit file a g
    
let usage () =
  prerr_endline
    "Usage: ./celaut option filename\nOptions:\tgrow\n\t\tstable\n\t\tedit"
    
let main () =
  if Array.length (Sys.argv) != 3
  then (usage(); exit(1));
  try
    let n,a,g = parse Sys.argv.(2) in
    match Sys.argv.(1) with
    |"grow" -> initscreen(); showgrowth a g
    |"stable" -> initscreen(); showstable a n
    |"edit" -> initscreen(); edit (Sys.argv.(2)) a g
    |_ -> usage(); exit(1)
  with
  |Corrupted_file(s) -> prerr_endline("Parsing failure: "^s);  exit(1)
  |System_limits(s)|Limits(s) -> prerr_endline("Limit exceeded: "^s);  exit(1)
  |exn -> exit(0)
						     
let m = main()
