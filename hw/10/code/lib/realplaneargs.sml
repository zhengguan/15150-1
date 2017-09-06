functor RealPlaneArgs (S : SEQUENCE) :> PLANEARGS where Seq = S =
struct
  structure Seq = S

  structure ScalarCore : SCALARCORE =
  struct
    type scalar = real

    val plus : real * real -> real = op+
    val minus : real * real -> real = op-
    val times : real * real -> real = op*
    val divide : real * real -> real = op/
    val compare : real * real -> order  = Real.compare
    fun fromRatio (x : IntInf.int, y : IntInf.int) =
        Real.fromLargeInt x / Real.fromLargeInt y
    fun toString (x : real) =  Real.fmt (StringCvt.SCI (SOME 4)) x
  end

  structure Scalar = MakeScalar(ScalarCore)

  fun distance (x1, y1) (x2, y2) =
      let
        val (dx,dy) = (x2 - x1, y2 - y1)
      in
        (Math.sqrt (dx * dx + dy * dy))
      end
end
