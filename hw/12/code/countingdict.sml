functor AddCounting (D : DICT) : COUNTING_DICT =
struct

  structure Key : ORDERED = D.Key

  (* Change this type as necessary *)
  type 'a dict = 'a D.dict * (int ref) D.dict

  (* build : (Key.t * 'a) list -> 'a dict *)
  (* REQUIRES: true *)
  (* ENSURES: Builds a counting dictionary by inserting a list of key-value
   * pairs into an initially empty dictionary *)
  fun build ([] : (Key.t * 'a) list) : 'a dict = (D.empty, D.empty)
    | build ((k,v)::L : (Key.t * 'a) list) : 'a dict =
      let
        val (d1,d2) = build(L)
      in
        (D.insert d1 (k,v), D.insert d2 (k, ref 0))
      end
  
  (* lookup : 'a dict -> Key.t -> 'a option *)
  (* REQUIRES: true *)
  (* ENSURES: lookup d k looks up the key k in the dictionary d *)
  fun lookup ((d1,d2) : 'a dict) (k : Key.t) : 'a option =
      let
        val _ = case (D.lookup d2 k) of
                    SOME(v) => v := !v + 1
                  | _ => ()
      in
        D.lookup d1 k
      end
  
  (* hits : 'a dict -> Key.t -> int *)
  (* REQUIRES: true *)
  (* ENSURES: hits d k evaluates to the number of times the key k has been
   * successfully looked up in the dictionary d *)
  fun hits ((d1,d2) : 'a dict) (k : Key.t) : int =
      let
        val SOME(v) = D.lookup d2 k
      in
        !v
      end
            
end


structure TestAddCounting =
struct

  structure IntOrder = 
  struct
    type t = int
    val compare = Int.compare
  end
  
  structure TreeDictAddCounting = AddCounting(TreeDict(IntOrder))
    
  (* Tests for AddCounting *)
  val testDict = TreeDictAddCounting.build([(0,1), (1,1), (2,5), (4,2)])
  val SOME(1) = TreeDictAddCounting.lookup testDict 0
  val SOME(1) = TreeDictAddCounting.lookup testDict 1
  val SOME(1) = TreeDictAddCounting.lookup testDict 1
  val SOME(2) = TreeDictAddCounting.lookup testDict 4
  val SOME(2) = TreeDictAddCounting.lookup testDict 4
  val SOME(2) = TreeDictAddCounting.lookup testDict 4
  val 1 = TreeDictAddCounting.hits testDict 0
  val 2 = TreeDictAddCounting.hits testDict 1
  val 0 = TreeDictAddCounting.hits testDict 2
  val 3 = TreeDictAddCounting.hits testDict 4
  
end
  
