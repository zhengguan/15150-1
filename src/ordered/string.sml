structure StringOrder : ORDERED =
struct
  type t = string

  val compare : t * t -> order = String.compare
end
