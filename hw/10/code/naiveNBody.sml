functor NaiveNBody (A : BHArgs) =
struct
(*   structure A = A *)
  structure Plane = A.BB.Plane
  structure Mech = A.Mech
  structure S = A.BB.Seq
  open Mech

  (* This is a quadratic pairwise computation of the acceleration vectors,
   * much like presented in lecture *)
  fun accelerations (bodies : body S.seq) : Plane.vec S.seq =
      S.map (fn b1 => Plane.sum (fn b2 => accOn (b1, b2)) bodies) bodies
end

(* this functor should produce a result that ascribes to a signature giving
just accelerations, and BarnesHut should do the same. We omit such a
signature so that you can test the components of your BarnesHut
implementation independently; it's very hard to debug code when the only
observable incorrect behaivour is "it produces incorrect transcripts
sometimes" *)
