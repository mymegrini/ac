type state = int;;

type generation = state array array;;

type compass = (int*int) array;;

type automaton = {
    vicinity : compass;
    rules : state array;
  }
;;

type formula = int list list;;

open IO;;

let parse cin =
  
