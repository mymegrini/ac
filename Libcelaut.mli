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
;;
type formula = string list
type file = string
  

exception Corrupted_file of string
exception System_limits of string
exception Limits of string
  
val parse : file -> int*automaton*generation

val show_generation : generation -> unit

val next_generation : automaton -> generation -> generation

val stables : automaton -> int -> formula

val flush : file -> automaton -> generation -> unit

val minisat : automaton -> int -> generation option

val show_stables : automaton -> int -> unit
