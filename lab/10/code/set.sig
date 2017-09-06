signature SET = 
sig
    type elem
    type set

    (* return an empty set *)
    val empty : unit -> set

   	(* return whether set is an empty set or not *)
   	val void : set -> bool

   	(* return whether an element is in the set or not *)
   	val find : elem -> set -> bool

   	(* insert an element into the set *)
   	val insert : elem -> set -> set

   	(* delete an element from the set *)
   	val delete : elem -> set -> set

   	(* return a set containing all elements in s1 and s2*)
   	val union : set -> set -> set

   	(* return a set containing elements in both s1 and s2 *)
   	val intersection : set -> set -> set

   	(* return elements in s1 and not in s2 *)
   	val difference : set -> set -> set

	(* return a list containing all elements in s *)
	val toList : set -> elem list

	(* return a set containing all elements in L *)
	val fromList : elem list -> set

end
