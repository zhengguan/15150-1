functor TreeDict (K : ORDERED) : DICT =
struct

  structure Key = K

  (* Invariant: BST *)
  datatype ('k, 'v) tree =
    Empty
  | Node of ('k, 'v) tree * ('k * 'v) * ('k, 'v) tree

  type 'v dict = (Key.t, 'v) tree

  val empty = Empty
  
  (* insert : 'v dict -> (Key.t * 'v) -> 'v dict *)
  (* REQUIRES: true *)
  (* ENSURES: insert D (k,v) evaluates to D with the mapping (k,v) added *)
  fun insert (Empty : 'v dict) (k : Key.t, v : 'v) : 'v dict = Node(Empty,(k,v),Empty)
    | insert (Node(l,(x,y),r) : 'v dict) (k : Key.t, v : 'v) : 'v dict = 
      case Key.compare(k, x) of 
          EQUAL => Node(l,(k,v),r)
        | LESS => insert l k
        | GREATER => insert r k
  
  (* lookup : 'v dict -> Key.t -> 'v option *)
  (* REQUIRES: true *)
  (* ENSURES: lookup D k evaluates to SOME v if k maps to v in D
   * and NONE otherwise *)               
  fun lookup (Empty : 'v dict) (k : Key.t) : 'v option = NONE
    | lookup (Node(l,(x,y),r) : 'v dict) (k : Key.t) : 'v option = 
      case Key.compare(k, x) of 
          EQUAL => SOME Node(l,(x,y),r)
        | LESS => lookup l k
        | GREATER => lookup r k

  (* removeHelp : 'v dict -> (Key.t * 'v) -> 'v dict *)
  (* REQUIRES: For remove(L,R) Key.compare(a,b) = LESS for all a in L and b in R *)
  (* ENSURES: remove(L,R) merges L and R *)  
  fun removeHelp (Empty : 'v dict, R : 'v dict) : 'v dict = R
    | removeHelp (Node(l1,(x,y),l2 : 'v dict, R : 'v dict) : 'v dict =
      Node(l1,(x,y),removeHelp(l2,R))
  
  (* remove : 'v dict -> Key.t -> 'v dict *)
  (* REQUIRES: true *)
  (* ENSURES: remove D k evaluates to D wtih the mapping for k removed *)     
  fun remove (Empty : 'v dict) (k : Key.t) : 'v dict = Empty
    | remove (Node(l,(x,y),r) : 'v dict) (k : Key.t) : 'v dict = 
      case Key.compare(k, x) of 
          EQUAL => removeHelp(l, r)
        | LESS => remove l k
        | GREATER => remove r k
  
  (* map : ('u -> 'v) -> 'u dict -> 'v dict *)
  (* REQUIRES: true *)
  (* ENSURES: map g D evaluates to a dicionary D' with values of type 'v such that
   * if a key is mapped to v in D then it is mapped to g v in D' *)                   
  fun map (g : 'u -> 'v) (Empty : 'u dict) : 'v dict = Empty
    | map (g : 'u -> 'v) (Node(l,(x,y),r) : 'u dict) : 'v dict =
      Node((map g l), (x, g y), (map g r))
  
  (* filter : ('v -> bool) -> 'v dict -> 'v dict *)
  (* REQUIRES: true *)
  (* ENSURES: filter p D devalutes to a dictionary D' such that if a key
   * is mapped to v in D and pv evalutes to true then it is mapped to v in D' *)    
  fun filter (p : 'v -> bool) (Empty : 'v dict) : 'v dict = Empty
    | filter (p : 'v -> bool) (Node(l,(x,y),r) : 'v dict) : 'v dict = 
      case p y of
          true => Node((filter l),(x,y),(filter r))
        | false => remove (Node((filter l),(x,y),(filter r))) x

end


structure TestFunDict =
struct

  structure IntOrder : ORDERED =
  struct
    type t = int
    val compare : t * t -> order = Int.compare
  end

  structure StringOrder : ORDERED =
  struct
    type t = string
    val compare : t * t -> order = String.compare
  end
  
  structure IntDict = TreeDict (IntOrder : ORDERED)    
  
  structure StrDict = TreeDict (StringOrder : ORDERED)

  (* Tests on IntDict *)
  val sipairs = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5)]
  val sd5 = List.foldl (fn (p, d) => StrDict.insert d p) StrDict.empty sipairs

  val SOME 3 = StrDict.lookup sd5 "three"
  val SOME 5 = StrDict.lookup sd5 "five"

  val sd4 = StrDict.remove sd5 "four"

  val NONE = StrDict.lookup sd4 "four"
  val SOME 5 = StrDict.lookup sd4 "five"

  val ssd5 = StrDict.map Int.toString sd5

  val NONE = StrDict.lookup ssd5 "six"
  val SOME "5" = StrDict.lookup ssd5 "five"

  val sd3 = StrDict.filter (fn x => x > 2) sd5

  val NONE = StrDict.lookup sd3 "one"
  val SOME 3 = StrDict.lookup sd3 "three"
  val SOME 5 = StrDict.lookup sd3 "five"

  (* Tests on IntDict *)
  val ispairs = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
  val id5 = List.foldl (fn (p, d) => IntDict.insert d p) IntDict.empty ispairs

  val SOME "three" = IntDict.lookup id5 3
  val SOME "five"  = IntDict.lookup id5 5

  val id4 = IntDict.remove id5 4

  val NONE = IntDict.lookup id4 4
  val SOME "five" = IntDict.lookup id4 5

end
