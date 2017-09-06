structure IntOrder : ORDERED =
struct
  type t = int

  val compare : t * t -> order = Int.compare
end

(* some synonyms *)
structure IntLt : ORDERED = IntOrder

structure IntInfLt : ORDERED =
struct
  type t = IntInf.int
  open IntInf
end
