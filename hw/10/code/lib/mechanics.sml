functor Mechanics (Plane : SPACE) : MECHANICS =
struct

  structure Plane = Plane

  val ++ = Plane.++
  val ** = Plane.**
  val --> = Plane.-->

  infixr 3 ++
  infixr 4 **
  infixr 3 -->

  (* ---------------------------------------------------------------------- *)
  (* n body *)
  (* throughout, we use SI units:
   mass is kg, length is meters, time is seconds, so force is Newtons *)

  (* (mass, position at time t, velocity at time t - (dt/2)),
   * where "dt" is the time step for the simulation.
   *
   * The position & velocity are out-of-sync because we are using "leapfrog
   * integration" to calculate the positions & velocities. However, you (the
   * student) should not need to worry about this, as it does not affect code
   * you will have to write.
   *)
  type body = Plane.Scalar.scalar * Plane.point * Plane.vec

  (* Projects the position field from a body *)
  fun position ((_,p,_) : body) = p

  fun bodyToString (_,p,v) = "P: " ^ (Plane.pointToString p) ^ "\t " ^
      "V: " ^ (Plane.vecToString v)

  (* Gravitational constant G = 6.67428E~11 N (m/kg)^2 *)
  val G = Plane.Scalar.divide (Plane.Scalar.fromRatio (667428, 100000),
                          Plane.Scalar.fromRatio (100000000000,1))

  (* acceleration on a body at p1 due to a body of mass m2 at p2;

   math:
   magnitude:
   a1 = F / m1
   F = G m1 m2 / r^2 where r is the distance between them
   so a1 = G m2 / r^2, cancelling m1
   direction: the direction from body 1 to body 2
   *)
  fun accOnPoint (p1 : Plane.point,
                  (m2, p2) : Plane.scalar * Plane.point) : Plane.vec =
      case Plane.pointEqual(p1 , p2) of
          true => Plane.zero
        | false =>
              let
                  val disp12 : Plane.vec = (p1 --> p2)
                  val r = Plane.mag disp12
                  val aMag = Plane.Scalar.divide (Plane.Scalar.times (G, m2),
                                                  Plane.Scalar.times (r, r))
                  val aDir = Plane.unitVec disp12
              in
                  aDir ** aMag
              end

  (* acceleration on body 1 due to body 2;
   * note: this is independent of the velocity of the bodies and also
   * independent ofthe mass of body 1.
   *)
  fun accOn ((_, p1, _) : body , (m2, p2, _) : body) : Plane.vec =
      accOnPoint (p1, (m2, p2))

  (* stepBody (b, a, t) computes the new position and velocity of b
   * after a t seconds given that a is the acceleration.
   * We use leapfrog integration.
   *)
  fun stepBody ((m, p, v) : body, a : Plane.vec, t : Plane.scalar) : body =
      let
        val v_new = v ++ a ** t
      in
        (m,
         Plane.displace (p, v_new ** t), (* leapfrog *)
         v_new)
      end

end
