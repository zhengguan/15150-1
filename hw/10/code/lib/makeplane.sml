functor MakePlane(P : PLANEARGS) :> SPACE where Seq = P.Seq
                                            and Scalar = P.Scalar =
struct
  structure Seq = P.Seq
  structure Scalar = P.Scalar

  type scalar = Scalar.scalar

  datatype coord = Coord of scalar * scalar
  datatype vector = Vec of scalar * scalar

  type point = coord
  type vec = vector

  infixr 3 ++

  (* v1 ++ v2 evaluates to the sum of the vectors *)
  fun (Vec (x1,y1)) ++ (Vec (x2,y2)) : vec =
      Vec (Scalar.plus (x1, x2), Scalar.plus (y1, y2))

  infixr 4 **

  (* v ** c evaluates to the scalar product of v with c *)
  fun (Vec (x,y) : vec) ** (c : scalar): vec =
      Vec (Scalar.times (x, c), Scalar.times (y, c))

  infixr 3 -->
  (* X --> Y is the vector from X to Y
   * computed by Y - X componentwise
   *)
  fun (Coord (x1, y1) : point) --> (Coord (x2, y2) : point) : vec =
      Vec (Scalar.minus (x2, x1), Scalar.minus (y2, y1))

  infixr 3 //
  (* v // c evaluates to the scalar product of v with (1/c) *)
    fun (v : vec) // (c : scalar) : vec = v ** (Scalar.invert c)

  (* The origin point *)
  val origin : point = Coord (Scalar.fromRatio (0,1), Scalar.fromRatio (0,1))

  (* Computes the cartesian coordinates of the given point *)
  fun cartcoord (Coord (x, y) : point) : scalar * scalar = (x,y)

  (* Return a point in 2D space with the given Cartesian coordinates *)
  fun fromcoord ((x, y) : scalar * scalar) = Coord (x, y)

  (* Computes the distance between the argument points *)
  fun distance (Coord x) (Coord y) = P.distance x y

  (* Computes the magnitude of the given vector *)
  fun mag (Vec (x, y) : vec) : scalar = distance (Coord (x, y)) origin

  fun vecToString (Vec (x,y) : vec) =
     "(" ^ Scalar.toString x ^ ", " ^ Scalar.toString y ^ ")"

  fun pointToString (Coord (x,y) : point) =
    "(" ^ Scalar.toString x ^ ", " ^ Scalar.toString y ^ ")"

  (* Tests two points for equality *)
  fun pointEqual (Coord (x1, y1) : point, Coord (x2, y2) : point) : bool =
    case (Scalar.compare(x1,x2), Scalar.compare(y1,y2)) of
        (EQUAL,EQUAL) => true
       |_ => false

  fun displace (Coord (x,y) : point, Vec (v1, v2) : vec) : point =
      Coord (Scalar.plus (x, v1), Scalar.plus (y, v2))

  val zero : vec = Vec (Scalar.zero,Scalar.zero)

  fun unitVec (V as Vec v : vec) : vec = V ** (Scalar.invert (mag V))

  fun sum (f : 'a -> vec) : 'a Seq.seq -> vec = Seq.mapreduce f zero op++

  (* compute the mid-point of a line that connects two points *)
  fun midpoint (Coord (x1, y1) : point) (Coord (x2, y2) : point) : point =
      Coord (Scalar.divide (Scalar.plus (x1, x2), Scalar.fromInt 2),
             Scalar.divide (Scalar.plus (y1, y2), Scalar.fromInt 2))

  (* Compute the point corresponding to the dispacement by the given vector
   * from the origin
   *)
  fun head (v : vec) : point = displace (origin, v)
end
