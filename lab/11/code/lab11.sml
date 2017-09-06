structure SeqAdditions =
struct
  type 'a seq = 'a Seq.seq

  (* Task 3.1 : seqFromList : 'a list -> 'a seq
   * REQUIRES: true
   * ENSURES: creates a sequence from a list
   *)
  fun seqFromList ([] : 'a list) : 'a Seq.seq = Seq.empty()
    | seqFromList (x::L : 'a list) : 'a Seq.seq =
      Seq.append (Seq.singleton x) (seqFromList(L))

  (* Task 3.2 : seqToList : 'a seq -> 'a list
   * REQUIRES: true
   * ENSURES: creates a list from a sequence
   *)
  fun seqToList (s : 'a Seq.seq) : 'a list =
      Seq.mapreduce (fn x => [x]) [] (fn (L1,L2) => L1@L2) s

  (* Task 3.3 : seqExists : ('a -> bool) -> ('a seq -> bool)
   * REQUIRES: true
   * ENSURES:
   *)
  fun seqExists (f : 'a -> bool) : 'a Seq.seq -> bool =
      Seq.mapreduce f false (fn (x,y) => x orelse y)

  (* Task 3.4 : acronym : (char Seq.seq Seq.seq) -> string
   * REQUIRES: true
   * ENSURES:
   *)
  fun acronym (s : char Seq.seq Seq.seq) : string =
      Seq.mapreduce (fn x => Char.toString(Seq.nth 0 x)) "" (fn (x,y) => x^y) s

  (* Task 4.1 : transpose : 'a seq seq -> 'a seq seq
   * REQUIRES:
   * ENSURES:
   *)
  fun transpose (s : 'a seq seq) : 'a seq seq =
      Seq.tabulate (fn i => 
                    Seq.tabulate (fn j => Seq.nth i (Seq.nth j s))
                                 (Seq.length s))
                   (Seq.length (Seq.nth 0 s))

  (* filter via map reduce *)
  (* Task 4.2 : filter : ('a -> bool) -> 'a seq seq -> 'a seq seq
   * REQUIRES:Seq.mapreduce (fn x => [x]) [] (fn (L1,L2) => L1@L2) s
   * ENSURES:
   *)
  fun filter' (p : 'a -> bool) (s : 'a seq) : 'a seq =    

  fun reduce' (g: 'a * 'a -> 'a) (b: 'a) (s: 'a seq) : 'a =
    case Seq.length s of
      0 => raise Fail "Unimplemented"
    | 1 => raise Fail "Unimplemented"
    | _ => raise Fail "Unimplemented"

end

structure FbTrees =
struct
  type 'a seq = 'a Seq.seq
	datatype 'a fbtree = Leaf of 'a | Node of ('a fbtree) seq;

	(* size : 'a fbtree -> int
	 * REQUIRES : T is an 'a fbtree
	 * ENSURES : the output of size T is the number of leaves in T
	 *)
	fun size (Leaf x) = 1
	  | size (Node s) = Seq.reduce (op +) 0 (Seq.map size s);

	(* Task 5.1 : depth : 'a fbtree -> int
	 * REQUIRES :
	 * ENSURES :
	 *)
	fun depth (Leaf x) = raise Fail "Unimplemented"
	  | depth (Node s) = raise Fail "Unimplemented"

	(* Task 5.2 : trav 'a fbtree -> 'a list
	 * REQUIRES :
	 * ENSURES :
	 *)
	fun trav (Leaf x) = raise Fail "Unimplemented"
	  | trav (Node s) = raise Fail "Unimplemented"

	(* fbmap ('a -> 'b) -> 'a fbtree -> 'b fbtree
 	 * REQUIRES : f is a total function
	 * ENSURES : the output of map f T is equivalent to applying f to all
	 			the leaves of T*)
	fun fbmap f (Leaf x) = Leaf (f x)
	  | fbmap f (Node s) = Node (Seq.map(fbmap f) s)

	(* Task 5.3 fbreduce ('a * 'a -> 'a) -> 'a -> 'a fbtree -> 'a
	 * REQUIRES :
	 * ENSURES :
	 *)
	fun fbreduce g z (Leaf x) = raise Fail "Unimplemented"
	  | fbreduce g z (Node s) = raise Fail "Unimplemented"

end

structure TestSeqAdditions =
struct
	open SeqAdditions

  (* your tests here *)

end

structure TestFbTrees =
struct
	open FbTrees

  (* your tests here *)

end
