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

  (* myAppend : 'a Seq.seq * 'a Seq.seq -> 'a Seq.seq
   * REQUIRES: true
   * ENSURES: myAppend s1 s2 == s, where s_i == s1_1 for 0<=i<length s1,
   * s2_i for length s1<=i<length s1 + length s2
   *)
  fun myAppend (s1 : 'a Seq.seq, s2 : 'a Seq.seq) : 'a Seq.seq =
      Seq.tabulate (fn i => (case i < Seq.length s1 of
                               true => Seq.nth i s1
                             | false => Seq.nth (i - (Seq.length s1)) s2))
                   (Seq.length s1 + Seq.length s2)

  (* Tests for myAppend *)
  val true = seqEq op= (seqFromList [],
                        myAppend (Seq.empty (), Seq.empty ()))
  val true = seqEq op= (seqFromList [1,2,3],
                        myAppend (Seq.empty (), seqFromList [1,2,3]))
  val true = seqEq op= (seqFromList [1,2,3,4],
                        myAppend (seqFromList [1,2], seqFromList [3,4]))
  val true = seqEq op= (seqFromList [1,2,3,4],
                        myAppend (seqFromList [1,2,3,4], Seq.empty ()))
  val false = seqEq op= (seqFromList [1,2,3,4,5],
                         myAppend (seqFromList [1,3,5], seqFromList [2,4]))

  (* max4 : ('a * 'a -> order) -> (('a * 'a) * ('a * 'a)) -> ('a * 'a)
     REQUIRES: true
     ENSURES: returns the two largest elements of the four
   *)
  fun max4 (cmp : 'a * 'a -> order) (((a, b), (c, d)) : ('a * 'a) * ('a * 'a))
      : ('a * 'a) =
      let
        val (max1, L) = maxL cmp [a, b, c, d]
      in
        case length L of
          3 => let val (max2, L') = maxL cmp L
               in (max1,max2) end
         |_ => (max1,max1)
      end

  (* INSERT TESTS HERE *)

  (* twoLargest : ('a * 'a -> order) -> ('a * 'a) -> 'a Seq.seq -> ('a * 'a)
   * REQUIRES: true
   * ENSURES: returns the two largest elements amongst s and (a, b)
   *)
  fun twoLargest (cmp : 'a * 'a -> order) ((a , b) : 'a * 'a) (s : 'a Seq.seq) : ('a * 'a) =
      let
        fun min (x, y) = case cmp (x,y) of
                           LESS => x
                          | _ => y
      in
        Seq.mapreduce (fn i => (i, min (a,b))) (a,b) (max4 cmp) s
      end


  (* atEdge : int Seq.seq Seq.seq -> int * int -> bool
   * REQUIRES: s is n by n and i, j are between 0 and n - 1 inclusive
   * ENSURES: returns whether the point is at an edge of the farm
   *)
  fun atEdge (s : int Seq.seq Seq.seq) (i : int, j : int) : bool =
      i = 0 orelse i = (Seq.length s) - 1 orelse
      j = 0 orelse j = (Seq.length s) - 1

  (* Tests *)
  val true = atEdge lonelyFarm (0, 0)
  val true = atEdge smallFarm (2, 1)
  val false = atEdge smallFarm (1, 1)


  (* isSink : int Seq.seq Seq.seq -> int * int -> bool
   * REQUIRES: s is n by n and i, j are between 0 and n - 1 inclusive
   * ENSURES: returns whether the point is a sink on the farm
   *)
  fun isSink (s : int Seq.seq Seq.seq) (i : int, j : int) : bool =
      let
        fun get (x, y) = Seq.nth y (Seq.nth x s)
        val sink = get (i, j)
      in
        not (atEdge s (i, j)) andalso
        List.foldl (fn (a, b) => a > sink andalso b) true
                   [get (i - 1, j), get (i + 1, j),
                    get (i, j - 1), get(i, j + 1)]
      end

  (* Tests *)
  val false = isSink bigFarm (0, 0)
  val false = isSink bigFarm (3, 1)
  val true = isSink bigFarm (1, 1)
  val true = isSink bigFarm (2, 2)


  (* rainfall : int Seq.seq Seq.seq -> int Seq.seq
   * REQUIRES: elevations is n by n and consists of unique heights
   * ENSURES: returns the indices of the sinks in elevations
   *)
  fun rainfall (elevations : int Seq.seq Seq.seq) : (int * int) Seq.seq =
      let
        val n = Seq.length elevations
      in
        Seq.filter (fn (i, j) => isSink elevations (i, j))
                   (Seq.tabulate (fn i => (i div n, i mod n)) (n * n))
      end

  (* Tests *)
  val true = seqEq op= (seqFromList [], rainfall emptyFarm)
  val true = seqEq op= (seqFromList [], rainfall lonelyFarm)
  val true = seqEq op= (seqFromList [(1,1)], rainfall smallFarm)
  val true = seqEq op= (seqFromList [(1,1), (2,2), (3,3)], rainfall bigFarm)

end
