signature POORMEMOIZER =
sig
  (* used to store the mapping *)
  structure D : DICT

  (* given a function, returns a poorly memoized version of that function *)
  val memo :  (D.Key.t -> 'a) -> (D.Key.t -> 'a)
end

signature MEMOIZER =
sig
  (* used to store the mapping *)
  structure D : DICT

  (* given a function, returns a  memoized version of that function. *)
  val memo : ((D.Key.t -> 'a) -> (D.Key.t -> 'a)) -> (D.Key.t -> 'a)
end
