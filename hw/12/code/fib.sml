signature FIBO =
sig
  (* on input n, computes the nth Fibonacci number *)
  val fib : IntInf.int -> IntInf.int
end

structure Fibo : FIBO =
struct
  fun fib (n : IntInf.int) : IntInf.int =
      case n
       of 0 => 0
        | 1 => 1
        | _ => fib(n-2) + fib(n-1)
end
