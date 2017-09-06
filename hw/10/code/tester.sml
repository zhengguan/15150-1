functor TestUtil (BH : BARNESHUT) =
struct
  structure A = BH.Args

  open A.Mech
  structure Seq = A.BB.Seq
  structure P = A.BB.Plane
  structure BB = A.BB
  structure Mech = A.Mech
  structure Scalar = A.BB.Plane.Scalar

  val ++ = Plane.++
  val ** = Plane.**
  val --> = Plane.-->
  val // = Plane.//

  infixr 3 ++
  infixr 4 **
  infixr 3 -->
  infixr 3 //

  fun seqFromList (l : 'a list) : 'a Seq.seq = 
      List.foldr (fn (x,y) => Seq.cons x y) (Seq.empty()) l

  fun seqAll (f : 'a -> bool) (s : 'a Seq.seq) : bool = 
      Seq.mapreduce f true (fn (b1, b2) => b1 andalso b2) s

  fun seqEq (aEq : 'a * 'a -> bool) (s1 : 'a Seq.seq) (s2 : 'a Seq.seq) : bool =
      Seq.length s1 = Seq.length s2 andalso (seqAll aEq (Seq.zip (s1, s2)))

  fun boxEq (bb1, bb2) : bool = 
      seqEq Plane.pointEqual (BB.vertices bb1) (BB.vertices bb2)

  (* Note: Doesn't compare velocities as these are unaffected by compute_tree *) 
  fun bodyEq ((m1, p1, _) : body, (m2, p2, _) : body) : bool = 
      (Scalar.eq (m1, m2)) andalso Plane.pointEqual (p1, p2)

  fun bhtreeEq (t1 : BH.bhtree, t2 : BH.bhtree) : bool = 
      case (t1, t2) of
        (BH.Empty, BH.Empty) => true 
      | (BH.Single b1, BH.Single b2) => bodyEq (b1, b2) 
      | (BH.Cell ((cm1, cp1), bd1, ts1), BH.Cell ((cm2, cp2), bd2, ts2)) => 
        Scalar.eq (cm1, cm2) andalso Plane.pointEqual (cp1, cp2)
        andalso Scalar.eq (bd1, bd2) andalso seqEq bhtreeEq ts1 ts2 
      | (_,_) => false

  (* some points and bounding boxes and bodies to use for testing *)
  val p00 = Plane.origin 
  val p44 = Plane.fromcoord (Scalar.fromInt 4, Scalar.fromInt 4)
  val p02 = Plane.fromcoord (Scalar.zero, Scalar.fromInt 2) 
  val p24 = Plane.fromcoord (Scalar.fromInt 2, Scalar.fromInt 4) 
  val p22 = Plane.fromcoord (Scalar.fromInt 2, Scalar.fromInt 2) 
  val p20 = Plane.fromcoord (Scalar.fromInt 2, Scalar.zero) 
  val p42 = Plane.fromcoord (Scalar.fromInt 4, Scalar.fromInt 2) 
  val p01 = Plane.fromcoord (Scalar.fromInt 0, Scalar.fromInt 1) 
  val p11 = Plane.fromcoord (Scalar.fromInt 1, Scalar.fromInt 1) 
  val p40 = Plane.fromcoord (Scalar.fromInt 4, Scalar.zero) 
  val p04 = Plane.fromcoord (Scalar.zero, Scalar.fromInt 4) 
  val p13 = Plane.fromcoord (Scalar.one, Scalar.fromInt 3) 
  val p33 = Plane.fromcoord (Scalar.fromInt 3, Scalar.fromInt 3)

  val bb0 : BB.box = BB.fromPoints (p02,p24) 
  val bb1 : BB.box = BB.fromPoints (p22,p44) 
  val bb2 : BB.box = BB.fromPoints (p00,p22)
  val bb3 : BB.box = BB.fromPoints (p20,p42) 
  val bb4 : BB.box = BB.fromPoints (p00,p44)

  val body1 : body = (Scalar.one, p40, Plane.zero) 
  val body2 : body = (Scalar.one, p22, Plane.zero) 
  val body3 : body = (Scalar.one, p04, Plane.zero)

end

functor Tester (BH : BARNESHUT) =
struct
  structure Util = TestUtil(BH)
  open Util
  
  (* sample tests using values in Util *)
  val false = Util.boxEq (Util.bb0, Util.bb1)
  val true = Util.bhtreeEq (BH.Empty, BH.Empty)

    (* Tests for barycenter *)
  val (a,b) = BH.barycenter(
              seqFromList([(Scalar.fromInt(2), p04),
                           (Scalar.fromInt(2),  p22)]))
  val (c,d) = BH.barycenter(
              seqFromList([(Scalar.one, p02),
                           (Scalar.one,  p20)]))
  val (e,f) = BH.barycenter(
              seqFromList([(Scalar.fromInt(2), p22),
                           (Scalar.zero,  p22)]))
  val true = Scalar.eq(Scalar.fromInt(4), a)
  val true = Plane.pointEqual(p13, b)
  val true = Scalar.eq(Scalar.fromInt(2), c)
  val true = Plane.pointEqual(p11, d)
  val true = Scalar.eq(Scalar.fromInt(2), e)
  val true = Plane.pointEqual(p22, f)

  (* Tests for quarters *)
  val corners = BH.quarters(bb4)
  val true = boxEq((Seq.nth 0 corners), bb0)
  val true = boxEq((Seq.nth 1 corners), bb1)
  val true = boxEq((Seq.nth 2 corners), bb2)
  val true = boxEq((Seq.nth 3 corners), bb3)

  (* Tests for compute_tree *) 
  val bodyTree = BH.compute_tree(
                 seqFromList([body1, body2, body3]))          
  val true = bhtreeEq(BH.Cell((Scalar.fromInt(3), p22),
                              Scalar.fromInt(8),
                              seqFromList([
                              BH.Cell((Scalar.fromInt(2), p13),
                                      Scalar.fromInt(4),
                                      seqFromList([
                                      BH.Single(body3), BH.Empty,
                                      BH.Empty, BH.Single(body2)])),
                              BH.Empty, BH.Empty, BH.Single(body1)])),
                              bodyTree)

  (* Tests for too_far *)
  val true = BH.too_far p11 p22 Scalar.one Scalar.one
  val true = BH.too_far p11 p22 (Scalar.fromInt(2)) Scalar.one
  val false = BH.too_far p11 p22 (Scalar.fromInt(3)) Scalar.one
  val true = BH.too_far p11 p22 (Scalar.fromInt(3)) (Scalar.fromInt(2))
  val true = BH.too_far p00 p44 (Scalar.fromInt(4)) Scalar.one

end

(* run our tests on rationals since they're reliable *)
structure RatTests = Tester(BarnesHut(RatArgs))
