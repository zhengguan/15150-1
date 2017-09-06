signature EQUAL = 
sig
    type t (* type of element *)
    
	(* returns true if t1 is equal to t2 and false otherwise *)
	val equal : t * t -> bool 
end

signature PAIR_OF_EQUAL =
sig
  structure E1 : EQUAL
  structure E2 : EQUAL
end
