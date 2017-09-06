functor SetOfSets (S : SET) : SET = 
struct
  
  	(* Impelement PowerSet here *)
	exception Unimplemented

  	structure DictEqual : EQUAL =
	struct
		(* write DictEqual here *)

		(* replace unit with the correct type *)
		type t = unit
		
		fun equal (A : t, B : t) : bool = raise Unimplemented

	end


end
