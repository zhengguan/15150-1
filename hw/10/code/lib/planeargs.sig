signature PLANEARGS =
sig
  structure Seq : SEQUENCE
  structure Scalar : SCALAR
  val distance : (Scalar.scalar * Scalar.scalar)
                 -> (Scalar.scalar * Scalar.scalar)
                 -> Scalar.scalar
end
