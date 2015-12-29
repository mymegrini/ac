type state = int;;

type generation = state array array;;

type compass = (int*int) array;;

type automaton = {
    vicinity : sphere;
    rules : state array;
  }
;;

type formula = int list list;;


