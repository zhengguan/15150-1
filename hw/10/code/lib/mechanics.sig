signature MECHANICS =
sig
  structure Plane : SPACE

  (* we represent bodies as (mass, location, velocity) *)
  type body = Plane.scalar * Plane.point * Plane.vec

  val position : body -> Plane.point
  val bodyToString : body -> string

  (* universal gravitational constant for this universe *)
  val G : Plane.scalar

  (* acceleration on a point due to a mass and a location *)
  val accOnPoint : Plane.point * (Plane.scalar * Plane.point) -> Plane.vec

  (* You shouldn't need these*)
  val accOn : body * body -> Plane.vec
  val stepBody : body * Plane.vec * Plane.scalar -> body
end
