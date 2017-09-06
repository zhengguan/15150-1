signature UTIL =
sig
  (* peelOff (s1,s2) = SOME s' if s2 = s1 ^ s'
   *                 = NONE otherwise
   *
   * Ex:
   *   peelOff ("a","a")  = SOME("")
   *   peelOff ("a","ab") = SOME("b")
   *   peelOff ("a","c")  = NONE
   *)
  val peelOff : string * string -> string option

  (* peelInt s = SOME (i,s') if the longest non-empty prefix of s
   *                comprised only of an optional leading #"~" and following digits 
   *                parses to the integer i, and s = i ^ s'
   *           = NONE otherwise
   *
   * Ex:
   *   peelInt "55hello"  = SOME(55,"hello")
   *   peelInt "~55hello" = SOME(~55,"hello")
   *   peelInt "-55hello" = NONE
   *   peelInt "hello55"  = NONE
   *   peelInt "12~100"   = SOME(12,"~100")
   *)
  val peelInt : string -> (int * string) option
end
