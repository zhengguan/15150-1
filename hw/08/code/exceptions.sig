signature FACTORIZE =
sig
  exception Prime
  val next_divisor : int * int -> int
  val factorizer : int -> int list
end