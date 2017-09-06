(* Remove this line when you've finished all tasks *)
exception Unimplemented

(* Contains some helpful functions for testing *)
structure SequenceHelper =
struct

  (* Returns the maximum element of a list and the remaining elements *)
  fun maxL (cmp : 'a * 'a -> order) (L : 'a list) : ('a * 'a list) = 
      let
        val max = List.foldl 
                      (fn(a, b) => case cmp(a, b) of GREATER => a | _ => b) 
                      (List.hd L) L
      in (max, List.filter (fn n => cmp(n, max) <> EQUAL) L) end

  (* Generates a sequence from a list *)
  fun seqFromList (l : 'a list) : 'a Seq.seq =
      Seq.tabulate (fn i => List.nth (l, i)) (List.length l)

  (* Generates a farm (int seq seq) from int list list *)
  fun farmFromList (l : int list list) : int Seq.seq Seq.seq =
      Seq.map seqFromList (seqFromList l)

  (* Checks if two sequences are equal, given equality function p *)
  fun seqEq (p: 'a * 'a -> bool) (s1 : 'a Seq.seq, s2 : 'a Seq.seq) : bool =
      if Seq.length s1 <> Seq.length s2 then false
      else Seq.mapreduce p true (fn (x,y) => x andalso y) (Seq.zip (s1, s2))

  (* Some farms for your testing convenience *)
  val emptyFarm = farmFromList []
  val lonelyFarm = farmFromList [[1]]
  val smallFarm = farmFromList [[1, 2, 3],
                                [4, 0, 5],
                                [6, 7, 8]]
  val bigFarm = farmFromList [[ 1, 10,  4,  5, 12],
                              [ 3,  0,  7, 13, 24],
                              [20,  8,  2, 21, 11],
                              [23, 22, 19,  6, 15],
                              [14, 17, 15,  9, 18]]

end

structure Sequences =
struct
  open SequenceHelper

  (* Task 4.1 *)
  (* myAppend : 'a Seq.seq * 'a Seq.seq -> 'a Seq.seq *)
  (* REQUIRES: true *)
  (* ENSURES: myAppend(s1,s2) = Seq.append s1 s2 *)
  fun myAppend (s1 : 'a Seq.seq, s2 : 'a Seq.seq) : 'a Seq.seq = 
    let
      val a = Seq.length s1
      val b = a + Seq.length s2
    in
      Seq.tabulate (fn i => case Int.compare(i, a) of
                                LESS => Seq.nth i s1
                              | _ => Seq.nth (i - a) s2)
                   b
    end
                      
  (* Task 4.2 *)
  (* max4 : (’a * ’a -> order) -> ((’a * ’a) * (’a * ’a)) -> (’a * ’a) *)
  (* REQUIRES: true *)
  (* ENSURES: max4 cmp ((a,b),(c,d)) returns the two largest elements of a,b,c,d
   * according to cmp  *)
  fun max4 (cmp : 'a * 'a -> order) (((a, b), (c, d)) : ('a * 'a) * ('a * 'a)) : ('a * 'a) =
      let
        val (x, L1) = maxL cmp [a,b,c,d]
        val (y, L2) = case (length(L1) = 3) of
                          true => maxL cmp L1
                        | _ => (x, [])
      in 
        (y,x)
      end

  (* Task 4.3 *)
  (* twoLargest : (’a * ’a -> order) -> (’a * ’a) -> ’a seq -> (’a * ’a) *)
  (* REQUIRES: true *)
  (* ENSURES: twoLargest cmp b s returns the two largest elements from b and s
   * according to cmp  *)
  fun twoLargest (cmp : 'a * 'a -> order) (b : 'a * 'a) (s : 'a Seq.seq) : ('a * 'a) =
      let
        val (b1, b2) = b
        val min = case cmp(b1, b2) of
                      LESS => b1
                    | _ => b2
      in
        Seq.reduce (max4 cmp) b (Seq.map (fn x => (x,min)) s)
      end
      
  (* Task 6.1 *)
  (* note that atEdge and isSink are optional, but probably useful *)
  
  (* atEdge : int Seq.seq Seq.seq -> (int * int) -> bool *)
  (* REQUIRES: true *)
  (* ENSURES: atEdge s (i,j) evaluates to to true if and only if
   * (i,j) is not an edge in s *)
  fun atEdge (s : int Seq.seq Seq.seq) (i : int, j : int) : bool =
      if (i <= 0 orelse i >= Seq.length s - 1 orelse j <= 0 orelse j >= Seq.length s - 1)
        then true
        else false
  
  (* isSink : int Seq.seq Seq.seq -> (int * int) -> bool *)
  (* REQUIRES: true *)
  (* ENSURES: isSink s (i,j) evaluates to to true if and only if
   * (i,j) is a sink in s *)
  fun isSink (s : int Seq.seq Seq.seq) (i : int, j : int) : bool =
      if atEdge s (i, j)
        then false
        else
          let
            val x = Seq.nth j (Seq.nth i s)
            val up = Seq.nth j (Seq.nth (i-1) s)
            val down = Seq.nth j (Seq.nth (i+1) s)
            val left = Seq.nth (j-1) (Seq.nth i s)
            val right = Seq.nth (j+1) (Seq.nth i s)
          in
            if (x < up andalso x < down andalso x < left andalso x < right)
              then true
              else false
          end
  
  (* rainfall : int Seq.seq Seq.seq -> (int * int) Seq.seq *)
  (* REQUIRES: true *)
  (* ENSURES: rainfall s evaluates to a sequence of sink positions in s *)
  fun rainfall (elevations : int Seq.seq Seq.seq) : (int * int) Seq.seq =
      let
        val n = Seq.length elevations
        val sinks = Seq.tabulate (fn x =>
                                  let val (ix, iy) = (x div n, x mod n)
                                  in (isSink elevations (ix, iy), ix, iy)
                                  end)
                                 (n * n) 
      in 
        Seq.map (fn (b, ix, iy) => (ix, iy))
                (Seq.filter (fn (b, ix, iy) => b) sinks)
      end
      
end

(* Tests for Sequences *)
structure TestSequences =
struct
  open SequenceHelper
  open Sequences

  (* Tests for myAppend *)
  val intSeqEq = seqEq (fn (a : int, b : int) => a = b)
  val true = intSeqEq(seqFromList([1,2,3,4,5,6]),
      myAppend(seqFromList([1,2,3]), seqFromList([4,5,6])))
  val true = intSeqEq(seqFromList([1,2,3]),
      myAppend(seqFromList([1]), seqFromList([2,3])))
  val true = intSeqEq(seqFromList([1,2,3,3,4,5]),
      myAppend(seqFromList([1,2,3]), seqFromList([3,4,5])))
  val true = intSeqEq(seqFromList([1,2,3]),
      myAppend(seqFromList([1,2,3]), seqFromList([])))
  val true = intSeqEq(seqFromList([4,5,6]),
      myAppend(seqFromList([]), seqFromList([4,5,6])))
  val true = intSeqEq(seqFromList([]),
      myAppend(seqFromList([]), seqFromList([])))
      
  (* Tests for max4 *)
  val (3,4) = max4 Int.compare ((1,4), (3,2))
  val (5,6) = max4 Int.compare ((3,4), (5,6))
  val (1,1) = max4 Int.compare ((0,1), (0,1))
  val (2,2) = max4 Int.compare ((2,2), (2,2))
  
  (* Tests for twoLargest *)
  val (4,5) = twoLargest Int.compare (1,5) (seqFromList([2,3,4]))
  val (4,5) = twoLargest Int.compare (1,2) (seqFromList([3,4,5]))
  val (4,5) = twoLargest Int.compare (4,5) (seqFromList([1,2,3]))
  val (0,1) = twoLargest Int.compare (0,0) (seqFromList([1,0,0]))
  val (2,2) = twoLargest Int.compare (2,2) (seqFromList([2,2,2]))
  val (1,2) = twoLargest Int.compare (1,2) (seqFromList([]))
  
  (* Tests for rainfall *)
  val intintSeqEq = seqEq (fn ((a,b) : int * int, (c,d) : int * int) =>
                        a = b andalso c = d)
  val true = intintSeqEq (seqFromList([]),
      rainfall emptyFarm)
  val true = intintSeqEq (seqFromList([]),
      rainfall lonelyFarm)
  val true = intintSeqEq (seqFromList([(1,1)]),
      rainfall smallFarm)
  val true = intintSeqEq (seqFromList([(1,1), (2,2), (3,3)]),
      rainfall bigFarm)  
   
end
