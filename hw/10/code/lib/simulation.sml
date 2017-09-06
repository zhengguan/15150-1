functor Simulation (BH : BARNESHUT) =
struct
  (*open BH.Mech
  structure Seq = BH.Seq*)
  open BH.Args.Mech
  structure Seq = BH.Args.BB.Seq

  (* t is the timestep *)
  fun stepGen (accs : body Seq.seq -> Plane.vec Seq.seq)
      (bodies : body Seq.seq , t : Plane.scalar) =
      Seq.map (fn (b,a) => stepBody (b,a,t))
      (Seq.zip (bodies,
                accs bodies))

  val bodySeqToString =
      String.concatWith "," o
      Seq.mapreduce (fn x => [Plane.pointToString (position x)]) [] op@

  fun compute_coordinates (s : body Seq.seq) (t : Plane.scalar) (niters : int)
                          (accs : body Seq.seq -> Plane.vec Seq.seq)
        : body Seq.seq list =
      let fun comp 0 s acc = (s :: acc)
            | comp n s acc =
              comp (n-1) (stepGen accs (s, t)) (s :: acc)
      in rev (comp niters s [])
      end

  fun seqFromList l = foldr (fn (x,y) => Seq.cons x y) (Seq.empty ()) l

  (* FIXME: toString for metrics may not be real-formatted.
   * Need to standardize on formatting.
   *)
  fun run (accs : body Seq.seq -> Plane.vec Seq.seq)
          (names : string list)
          (s : body list)
          (t : Plane.scalar)
          (niters : int)
          (file : string) : unit =
      let
          open TextIO
          val coords = compute_coordinates (seqFromList s) t niters accs
          val outfile = TextIO.openOut file
          fun pr x = output (outfile, x)
          fun prn x = pr (x ^ "\n")
      in
        (prn (String.concatWith "," names);
         List.app (prn o bodySeqToString) coords;
         flushOut outfile;
         closeOut outfile)
      end

  (* Use the quadratic time pairwise computation of acceleration vectors *)
  (* we change the prefix so it doesn't clobber the BH transcripts *)
  structure Naive = NaiveNBody(struct
                                open BH.Args
                                val prefix = "naive" ^ prefix
                                end)
  val runPairwise = run Naive.accelerations []

  (* Use Barnes Hut algorithm for the computation of acceleration vectors *)
  val runBH = run BH.accelerations []
end
