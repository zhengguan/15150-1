functor BarnesHut (A : BHArgs) : BARNESHUT =
struct

  structure Args = A

  (* helpful aliases to clean up the code below *)
  structure Seq = A.BB.Seq
  structure Plane = A.BB.Plane
  structure BB = A.BB
  structure Mech = A.Mech
  structure Scalar = A.BB.Plane.Scalar
  open A.Mech

  val ++ = Plane.++
  val ** = Plane.**
  val --> = Plane.-->
  val // = Plane.//

  infixr 3 ++
  infixr 4 **
  infixr 3 -->
  infixr 3 //

  (* this is the core idea of the barnes-hut algorithm *)
  (* a bhtree is a finitely branching tree of bodies that also
   * stores the center of mass and diameter of the bounding box
   * containing its children *)
  datatype bhtree =
      Empty
    | Single of body
    | Cell of (Plane.scalar * Plane.point) * Plane.scalar * (bhtree Seq.seq)
      (* ((mass, center), box diameter, sequence of length four 
                                        representing each quadrant) *)

  exception Unimplemented

  (* scale_point : Scalar.scalar * Plane.point -> Plane.vec*)
  (* ENSURES: Scales the vector from the origin to p by the factor m. *)
  fun scale_point (m : Scalar.scalar, p : Plane.point) : Plane.vec =
      (Plane.origin --> p) ** m

  (* Task 3.1 *)
  (* barycenter : (Scalar.scalar * Plane.point) Seq.seq
                -> Scalar.scalar * Plane.point *)
  (* REQUIRES : true *)
  (* ENSURES : barycenter s computes the pair (m, c) where m is the
   * total mass of the bodies in the sequence *)
  fun barycenter (s : (Scalar.scalar * Plane.point) Seq.seq) :
      Scalar.scalar * Plane.point = 
      let
        val m = Seq.mapreduce (fn (x,y) => x) Scalar.zero
                              (fn (x,y) => Scalar.plus(x,y)) s
        val c = Plane.head(Plane.sum
                           (fn (x,y) => scale_point(Scalar.divide(x, m), y))
                            s)
      in
        (m, c)
      end

  (* Testing hint: use seqFromList and seqEq and boxEq
     to make at least one test case.
     You may find the boxes defined above to be helpful. *)


  (* Task 3.2 *)
  (* quarters : BB.box -> BB.box Seq.seq *)
  (* REQUIRES : true *)
  (* ENSURES : quarters b computes a sequence of four bounding boxes
   * that correspond to the top-left, top-right, bottom-left,
   * and bottom-right quadrants of b *)
  fun quarters (bb : BB.box) : BB.box Seq.seq =
      let
        val center = BB.center(bb);
        val corners = BB.vertices(bb);
      in
        Seq.map (fn x => BB.fromPoints(x,center)) corners
      end        


  (* center_of_mass bhtree -> Scalar.scalar * Plane.point *)
  (* ENSURES
   * Projects the mass and center from the root node of a bhtree *)
  fun center_of_mass (T : bhtree) : Scalar.scalar * Plane.point =
      case T of
          Empty => (Scalar.zero, Plane.origin)
        | Single (m, p, _) => (m, p)
        | Cell (com, _, _) => com


  (* take_and_drop BB.box -> body Seq.seq -> (body Seq.seq, body Seq.seq)*)
  (* ENSURES take_and_drop bb s = (s1, s2) where every body in s1 is
   *   BB.inside bb, and every element in s2 is not BB.inside bb *)
  fun take_and_drop (bb : BB.box) (s : body Seq.seq)
      : body Seq.seq * body Seq.seq =
      let val inside = (fn (_,p,_) => BB.inside bb p)
      in
        (Seq.filter inside s, Seq.filter (fn b => not (inside b)) s)
      end


  exception InvariantViolation

  (* Task 3.3 *)
  (* compute_tree : body Seq.seq -> bhtree *)
  (* REQUIRES : true *)
  (* ENSURES : compute_tree s evaluates to the tree decomposition of s *)
  fun compute_tree (s : body Seq.seq) : bhtree =
      case Seq.length s of
          0 => Empty
        | 1 => Single (Seq.nth 0 s)
        | _ => let
                 val SOME bb = BB.hull (Seq.map position s)
                 val bd = BB.diameter(bb)
                 val boxes = quarters(bb)
                 val (nw, not) = take_and_drop (Seq.nth 0 boxes) s
                 val (ne, not) = take_and_drop (Seq.nth 1 boxes) not
                 val (sw, not) = take_and_drop (Seq.nth 2 boxes) not
                 val se = not
                 val bsq = Seq.append (Seq.singleton nw) (
                          Seq.append (Seq.singleton ne) (
                          Seq.append (Seq.singleton sw) (Seq.singleton se)))
                 val sq = Seq.map compute_tree bsq
                 val (m,c) = barycenter(Seq.map (fn (x,y,z) => (x,y)) s)
               in
                 Cell((m,c), bd, sq)
               end

  (* Testing hint: write at least one test case by
     working out the answer for (compute_tree bseq bb4).
     *)

  (* Task 3.4 *)
  (* too far : Plane.point -> Plane.point -> Scalar.scalar -> Scalar.scalar
             -> bool *)
  (* REQUIRES : true *)
  (* ENSURES : too far p c bd t evaluates to true if p a certain distance
   * away from a pseudobody at c *)
  fun too_far (p1 : Plane.point) (p2 : Plane.point) (bd : Plane.scalar)
              (t : Scalar.scalar) : bool =
      Scalar.lte(Scalar.divide(bd, (Plane.distance p1 p2)), t)

  (* Task 3.5 *)
  (* bh acceleration : bhtree -> Scalar.scalar -> body -> vec *)
  (* REQUIRES : true *)
  (* ENSURES : bh acceleration T threshold b computes the acceleration on b
   * from the tree *)
  fun bh_acceleration (Empty : bhtree) (t : Scalar.scalar) (b as (_, p, _))
      : Plane.vec = Plane.zero
    | bh_acceleration (Single body : bhtree) (t : Scalar.scalar) (b as (_, p, _))
      : Plane.vec = accOnPoint(p, barycenter(Seq.map (fn (x,y,z) => (x,y))
                                                     (Seq.singleton body)))
    | bh_acceleration (Cell((m,c), bd, sq) : bhtree) (t : Scalar.scalar) (b as (_, p, _))
      : Plane.vec =
      if (too_far p c bd t)
        then accOnPoint(p, (m, c))
        else Plane.sum (fn x => bh_acceleration x t b) sq
      

  (*
     barnes_hut : Plane.scalar -> body Seq.seq -> Plane.vec Seq.seq

   * ENSURES
     Given a threshold and a sequence of bodies, compute the acceleration
     on each body using the Barnes-Hut algorithm.
   *)
  fun barnes_hut (threshold : Plane.scalar) (s : body Seq.seq)
      : Plane.vec Seq.seq =
      Seq.map (bh_acceleration (compute_tree s) threshold) s

  val accelerations : body Seq.seq -> Plane.vec Seq.seq =
      barnes_hut (Plane.Scalar.fromRatio (A.thresh))
end

structure RatTrans = Transcripts(BarnesHut(RatArgs))
structure RealTrans = Transcripts(BarnesHut(RealArgs))
