functor BoundingBox (Plane : SPACE) :> BOX where Seq = Plane.Seq
                                            and Plane = Plane =
struct
  structure Plane = Plane
  structure Seq = Plane.Seq

  type box = Plane.point * Plane.point
           (* (ll, ur) where ll is the lower left corner of the box and
            *                ur is the upper right corner of the box
            *)

  fun toString ((p1, p2) : box) : string =
      "(" ^ (Plane.pointToString p1) ^ ", " ^ (Plane.pointToString p2) ^ ")"


  (* contained p bb evaluates to true if and only if the point p is in b *)
  fun inside ((lowleft, upright) : box)(p : Plane.point) : bool =
      let
        val (x, y) = Plane.cartcoord p
        val (xleft, ylow) = Plane.cartcoord lowleft
        val (xright, yup) = Plane.cartcoord upright
      in
        Plane.Scalar.gte (x, xleft) andalso Plane.Scalar.lte (x, xright) andalso
        Plane.Scalar.gte (y, ylow) andalso Plane.Scalar.lte (y, yup)
      end

  (* Returns the four corners of the bounding box in top left, top right,
   * bottom left, bottom right order *)
  fun vertices ((lowleft, upright) : box)
      : Plane.point Seq.seq =
      let
        val (xleft, ylow) = Plane.cartcoord lowleft
        val (xright, yup) = Plane.cartcoord upright
      in
        Seq.tabulate (fn 0 => Plane.fromcoord (xleft, yup)
                       | 1 => upright
                       | 2 => lowleft
                       | 3 => Plane.fromcoord (xright, ylow)
                       | _ => raise Fail "out of range")
                     4
      end

  (* addPoint (p, bb) returns the smallest bounding box containing both
   * p and all the points in bb
   *)
  fun addPoint (p : Plane.point, (lowleft, upright) : box) : box =
      let
        val (x, y) = Plane.cartcoord p
        val (xleft, ylow) = Plane.cartcoord lowleft
        val (xright, yup) = Plane.cartcoord upright
      in
        (Plane.fromcoord (Plane.Scalar.min (x, xleft),
                          Plane.Scalar.min (y, ylow)),
         Plane.fromcoord (Plane.Scalar.max (x, xright),
                          Plane.Scalar.max (y, yup)))
      end

  (* fromPoint p returns the smallest bounding box containing p
   * Namely, it returns the box consisting of the single point p
   *)
  fun fromPoint (p : Plane.point) : box = (p, p)

  (* union (bb1, bb2) returns the smallest bounding box containing both
   * all the points in bb1 and all the points in bb2
   *)
  fun union ((lowleft1, upright1) : box,
                (lowleft2, upright2) : box) : box =
      let
        val (xleft1, ylow1) = Plane.cartcoord lowleft1
        val (xright1, yup1) = Plane.cartcoord upright1
        val (xleft2, ylow2) = Plane.cartcoord lowleft2
        val (xright2, yup2) = Plane.cartcoord upright2
      in
        (Plane.fromcoord (Plane.Scalar.min (xleft1, xleft2),
                          Plane.Scalar.min (ylow1, ylow2)),
         Plane.fromcoord (Plane.Scalar.max (xright1, xright2),
                          Plane.Scalar.max (yup1, yup2)))
      end

  (* fromPoints (p1, p2) returns the smallest bounding box containing both
   * p1 and p2
   *)
  fun fromPoints (p1 : Plane.point, p2 : Plane.point) : box =
      union (fromPoint p1, fromPoint p2)

  (* Computes the center point of the bounding box *)
  fun center ((lowleft, upright) : box) : Plane.point =
      Plane.midpoint lowleft upright

  (* Computes the minimum bounding box of a sequence of points.
     or returns NONE if the sequence is empty
     *)
  fun hull (s : Plane.point Seq.seq) : box option =
      let fun join (b, NONE) = b
            | join (NONE, b) = b
            | join (SOME b1, SOME b2) = SOME (union (b1,b2))
      in Seq.mapreduce (SOME o fromPoint) NONE join s
      end
  
  (* computes the diameter of the bounding box *)
  fun diameter ((lowleft, upright) : box) : Plane.scalar =
      Plane.distance lowleft upright

end
