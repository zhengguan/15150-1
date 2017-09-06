structure SeqUtils :
sig
  (* assume sequence is non-empty *)
  val reduce1 : ('a * 'a -> 'a) -> 'a Seq.seq -> 'a
  val null : 'a Seq.seq -> bool

  val seqToList : 'a Seq.seq -> 'a list
  val seqFromList : 'a list -> 'a Seq.seq
end =
struct
    fun reduce1 (c : 'a * 'a -> 'a) (s : 'a Seq.seq) : 'a =
        let
            fun mergeo (x : 'a option , y : 'a option) : 'a option =
                case (x , y) of
                    (NONE , y) => y
                  | (x , NONE) => x
                  | (SOME x, SOME y) => SOME (c (x,y))
        in
            case Seq.mapreduce SOME NONE mergeo s of
                SOME x => x
              | NONE => raise Fail "called reduce1 on an empty sequence"
        end

  fun null s = Seq.length s = 0

  fun seqToList s = Seq.mapreduce (fn x => [x]) [] op@ s
  fun seqFromList l = List.foldr (fn (x,y) => Seq.cons x y) (Seq.empty()) l

end

structure TestUtil =
struct

    val test = Seq.cons (((), 84)) (Seq.cons ((), 32) (Seq.cons ((), ~4) (Seq.cons ((), 0) (Seq.empty()))))
    structure O = OrderUtils (PairSecondOrder (struct type left = unit structure Right = IntLt end))
    val test = SeqUtils.reduce1 O.min test

    val true = SeqUtils.null (Seq.empty ())
    val false = SeqUtils.null (Seq.tabulate (fn i => i) 1)
end
