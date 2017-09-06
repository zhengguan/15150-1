signature BOX =
sig
  structure Plane : SPACE
  structure Seq : SEQUENCE

  type box
  val toString : box -> string

  (* inside bb p evaluates to true if and only if the point p is in bb *)
  val inside : box -> Plane.point -> bool

  (* Returns the four corners of the bounding box in top left, top right,
   * bottom left, bottom right order *)
  val vertices : box -> Plane.point Seq.seq

  (* fromPoint p returns the smallest bounding box containing p
   * Namely, it returns the box consisting of the single point p
   *)
  val fromPoint : Plane.point -> box

  (* union (bb1, bb2) returns the smallest bounding box containing both
   * all the points in bb1 and all the points in bb2
   *)
  val union : box * box -> box

  (* fromPoints (p1, p2) returns the smallest bounding box containing both
   * p1 and p2
   *)
  val fromPoints : Plane.point * Plane.point -> box

  (* Computes the center point of the bounding box *)
  val center : box -> Plane.point

  (* Computes the minimum bounding box of a sequence of points,
     or returns NONE if the sequence is empty.
     *)
  val hull : Plane.point Seq.seq -> box option

  (* Finds the diameter of the bounding box (distance from
     top left to bottom right *)
  val diameter : box -> Plane.scalar
end
