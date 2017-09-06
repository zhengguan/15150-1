functor DictSet (Dict : DICT) : SET =
struct

  exception NotInSet

  type elem = Dict.key

  type set = unit Dict.dict

  val empty = Dict.empty

  fun void (S : set) : bool = Dict.size S = 0

  fun find (e : elem) (S : set) : bool =
    case Dict.find S e of
      NONE => false
      | _ => true

  fun insert (e : elem) (S : set) : set =
    Dict.insert (fn (x,y) => ()) (e,()) S

  fun delete (e : elem) (S : set) : set =
    Dict.delete e S

  fun union (s1 : set) (s2 : set) : set =
    Dict.merge (fn (x,y) => ()) (s1, s2)

  fun intersection (s1 : set) (s2 : set) : set =
    Dict.filterk (fn x => find x s1) s2

  fun difference (s1 : set) (s2 : set) : set =
    Dict.filterk (fn x => not (find x s1)) s2

  fun toList (s : set) : elem list =
    List.map (fn(x,y) => x) (Dict.toList s)

  fun fromList (L : elem list) : set =
    Dict.fromList (List.map (fn x => (x,())) L)

end
