functor DictSet (Dict : DICT) : SET =
struct
	exception NotInSet
	exception Unimplemented

	(* replace unit with the correct types here *)
	type elem = Dict.key
	type set = unit Dict.dict

	fun empty() = Dict.empty()

	fun void S = Dict.size(S) = 0

	fun find e S = (Dict.find S e) <> NONE
	fun insert e S = Dict.insert (fn (x,y) => ()) (e, ()) S
	fun delete e S = Dict.delete e S
	
	fun union s1 s2 = Dict.merge (fn (x,y) => ()) (s1, s2)
	fun intersection s1 s2 = Dict.filterk (fn e => find e s1) s2
	fun difference s1 s2 = Dict.filterk (fn e => not(find e s1)) s2

	fun toList s =  map (fn (k,v) => k) (Dict.toList s)
	fun fromList L = Dict.fromList(map (fn k => (k,())) L)

end
