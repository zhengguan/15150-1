(* some instances of ORDERED for dictionaries *)
signature ORDERED =
sig
  type t
  val compare : t * t -> order
end

structure IntOrder : ORDERED =
struct
  type t = int

  val compare : t * t -> order = Int.compare
end

structure StringOrder : ORDERED =
struct
  type t = string

  val compare : t * t -> order = String.compare
end
