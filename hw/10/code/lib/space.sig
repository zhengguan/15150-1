(* scalars and operations on them *)
signature SPACE =
sig
  structure Scalar : SCALAR
  structure Seq : SEQUENCE

  type scalar = Scalar.scalar

  (* points and vectors and operations on them *)
  type point
  type vec

  (* v1 ++ v2 evaluates to the sum of the vectors *)
  val ++ : vec * vec -> vec

  (* v ** c evaluates to the scalar product of v with c *)
  val ** : vec * scalar -> vec

  (* X --> Y is the vector from X to Y *)
  val --> : point * point -> vec

  (* v // c evaluates to the scalar product of v with (1/c) *)
  val // : vec * scalar -> vec

  (* the center of the plane *)
  val origin : point

  (* Computes the distance between the argument points *)
  val distance : point -> point -> scalar

  (* Computes the magnitude of the given vector *)
  val mag : vec -> scalar

  val vecToString : vec -> string
  val pointToString : point -> string

  (* equality of points *)
  val pointEqual : point * point -> bool

  (* displace a point by a vector *)
  val displace : point * vec -> point

  (* the head of a vector as a point in the plane *)
  val head : vec -> point

  (* given a vector in a direction, produces the unit vec in that direction *)
  val unitVec : vec -> vec

  (* the zero vector *)
  val zero : vec


  (* given two points, gives the point at the middle of the line between them *)
  val midpoint : point -> point -> point

  (* sums up a bunch of vectors when you give it a function that tells
   * how to make a vector from a value of type 'a  *)
  val sum : ('a -> vec) -> 'a Seq.seq -> vec

  (* do NOT use these two functions except for testing *)
  val cartcoord : point -> scalar * scalar
  val fromcoord : scalar * scalar -> point
end
