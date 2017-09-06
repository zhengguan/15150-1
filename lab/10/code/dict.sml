functor Dict (K : EQUAL) : DICT =
struct
  structure K : EQUAL = K
  type 'a dict = (K.t*'a) list
  type key = K.t
  
  fun size (d : 'a dict) : int = List.length d

  fun find (d : 'a dict) (k : key) : 'a option = 
    case d of
      [] => NONE
      | (k1,v1)::ds => if K.equal(k1,k) then SOME v1 else
               find ds k

  fun insert (f : 'a*'a -> 'a) (k : key, v : 'a) (d : 'a dict) : 'a dict =
    case d of
      [] => [(k,v)]
      | (k1,v1)::xs => if K.equal(k1,k) then (k,f(v1,v))::xs
               else (k1,v1)::(insert f (k,v) xs)

  fun delete (k : key) (d : 'a dict) : 'a dict =
    case d of
      [] => []
      | (k1,v1)::xs => if K.equal(k1,k) then xs
               else (k1,v1)::delete k xs


  fun empty () = []

  fun singleton (k : key, v : 'a) : 'a dict = [(k, v)]
  
  fun map (f : 'a -> 'b) (d : 'a dict) : 'b dict = 
    List.map (fn (k,v) => (k,f v)) d

  fun filter (f : 'a -> bool) (d : 'a dict) : 'a dict =
    List.filter (fn (k,v) => f v) d

  fun filterk (f : key -> bool) (d : 'a dict) : 'a dict =
    List.filter (fn (k,v) => f k) d

  
  fun merge (f : 'a*'a -> 'a) (d1 : 'a dict, d2 : 'a dict) : 'a dict =
    let
      val d2' = List.filter 
        (fn (k,v) => case find d1 k of NONE => true | _ => false) d2
      val d1' = List.map (fn (k,v) => 
              case find d2 k of 
                NONE => (k,v) 
                | SOME v' => (k,f(v,v'))) d1
    in
      d1'@d2'
    end

  fun toList (d : 'a dict) : (key * 'a) list = d

  fun fromList (L : (key * 'a) list) : 'a dict = L
    
end
