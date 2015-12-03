open Pcore

exception Corrupted_file of string
  
val parse : in_channel -> int*automaton*generation
