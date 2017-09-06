signature BHArgs =
sig
  (* what the first bit of the file names should be *)
  val prefix : string

  (* threshold value for BH algorithm *)
  val thresh : IntInf.int * IntInf.int

  (* the model of the universe *)
  structure BB : BOX
  structure Mech : MECHANICS

  sharing BB.Plane = Mech.Plane
  sharing BB.Plane.Scalar = Mech.Plane.Scalar
  sharing BB.Plane.Seq = BB.Seq
end
