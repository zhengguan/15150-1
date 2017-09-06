structure SizeSeq : TSEQ =
struct
    (* remove this when you are done *)
    exception Unimplemented
  
    datatype 'a sseq = Empty
                     | Leaf of 'a
                     | Node of 'a sseq * int * 'a sseq

    type 'a seq = 'a sseq
    exception Range

    (* length : 'a seq -> int *)
    (* REQUIRES : true *)
    (* ENSURES : length S evaluates to the length of S *)
    fun length Empty = 0
      | length (Leaf _) = 1
      | length (Node(L,s,R)) = s
    
    (* nth : int -> 'a seq -> 'a *)
    (* REQUIRES : true *)
    (* ENSURES : nth i s evaluates to the ith item in S *)
    fun nth i Empty = raise Range
      | nth 0 (Leaf x) = x
      | nth i (Leaf x) = raise Range
      | nth i (Node(L,s,R)) =
          let
            val lenL = length L
            val lenR = length R
          in
            case (Int.compare(i, lenL + lenR), Int.compare(i, lenL)) of
                (LESS, LESS) => nth i L
              | (LESS, _) => nth (i - lenL) R
              | _ => raise Range
          end
          
    (* tabulate : int -> 'a seq -> 'a *)
    (* REQUIRES : n >= 0 *)
    (* ENSURES : Seq.tabulate f n evaluates to a sequence s with length n
     * where the ith item of s is the result of evaluating (f i) *)
    fun tabulate f 0 = Empty
      | tabulate f 1 = Leaf(f 0)
      | tabulate f n =
        Node(tabulate f (n div 2), n,
             tabulate (fn x => f (x + (n div 2))) (n - (n div 2)))

end


structure TestSizeSeq =
struct
  open SizeSeq
  
  (*Tests for ShrubSeq *)
  val ShrubOne = tabulate (fn x => x) 8
  val ShrubTwo = tabulate (fn x => 2*x) 7
  val ShrubThree = tabulate (fn x => ~x - 5) 1
  val ShrubFour = tabulate (fn x => x) 0
  val 8 = length ShrubOne
  val 7 = length ShrubTwo
  val 1 = length ShrubThree
  val 0 = length ShrubFour
  val 2 = nth 2 ShrubOne
  val 12 = nth 6 ShrubTwo
  val ~5 = nth 0 ShrubThree
  
end
