type state = char

type generation = state array array

type rule = string
	      
type automaton = rule list
  
type formula = int list list
 
val next_generation : automaton -> generation -> generation  

val stables : automaton -> int -> formula
