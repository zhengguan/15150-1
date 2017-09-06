structure RealArgs : BHArgs =
struct
  val prefix = "real"
  val thresh : IntInf.int * IntInf.int = (1,2)
  structure BB : BOX = BoundingBox(MakePlane(RealPlaneArgs(VectorSeq)))
  structure Mech : MECHANICS = Mechanics(BB.Plane)
end

structure RatArgs : BHArgs =
struct
  val prefix = "rat"
  val thresh : IntInf.int * IntInf.int = (1,2)
  structure BB : BOX = BoundingBox(MakePlane(RatPlaneArgs(VectorSeq)))
  structure Mech : MECHANICS = Mechanics(BB.Plane)
end

(* given a universe, this functor represents the solar system in it. when
   go is forced, it runs barneshut in that universe with various subsets of
   the solar system and produces transcript files in the current working
   directory. these canned examples are small enough that an arbitrary
   precision rational implementation of the universe should allow BH to
   terminate on them with in a few minutes
*)
functor Transcripts (BH : BARNESHUT) =
struct
  structure Plane = BH.Args.BB.Plane
  structure Solars = Solars(BH)
  structure Sim = Simulation(BH)

  (* suspend this computation so that you get to choose when it happens *)
  fun go () =
      let
        (* helpers to make input *)
        fun prep solars bef (x,y) =
            (x,
             solars,
             String.concatWith "." [BH.Args.prefix, bef, y, "auto", "txt"])

        fun days (x : int) = (x+1, (Int.toString(x+1)) ^ "day")

        (* some canned input--small enough to run with rationals, too*)
        val secs_in_day = (Plane.Scalar.fromInt 864000)

        val onebody_list = map (prep Solars.one_body "onebody")
                               [(1,"1day"), (14,"2weeks"), (365,"1yr")]

        val twobody_list = map (prep Solars.two_body "twobody")
                               (List.tabulate(6,days))

        val system_list = map (prep Solars.solar_system "system")
                              (List.tabulate(2,days))

        val all = onebody_list @ twobody_list @ system_list
        val all' = if BH.Args.prefix = "real" then 
                    let 
                      val one1000 = (prep Solars.one_body "onebody") (days 1000)
                      val two1000 = (prep Solars.two_body "twobody") (days 1000)
                      val sys1000 = (prep Solars.solar_system "system") (days 1000)
                    in all @ [one1000, sys1000, two1000] end
                  else all

        (* how to run BH on the input---notice that this won't run naive *)
        fun dorun (num_days, bod, name) =
            let
              val () = print ("made " ^ name ^ " ...")
              val t1 = Time.now ();
              val () = Sim.runBH bod secs_in_day num_days name
              val t2 = Time.now ();
              val delta = Time.-(t2,t1)
              val () = print (" in " ^ Time.toString delta ^ " seconds\n")
            in
              ()
            end
      in
        (* actually run BH on the input and make transcripts *)
        List.app dorun all'
      end
end
