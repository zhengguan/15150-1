(* the code in this file is provided for testing and task 4.2 *)
structure Fibo : FIBO =
struct
  fun fib (n : IntInf.int) : IntInf.int =
      case n
       of 0 => 0
        | 1 => 1
        | _ => fib(n-2) + fib(n-1)
end

functor PoorMemoizer (D : DICT) : POORMEMOIZER =
struct
  structure D = D

  fun memo (f : D.Key.t -> 'a) : D.Key.t -> 'a =
      let
        val hist : 'a D.dict ref = ref D.empty

        fun f_memoed x =
            case D.lookup (!hist) x
             of SOME(b) => b
              | NONE =>
                let
                  val res = f x
                  val _ = (hist := D.insert (!hist) (x,res))
                in
                  res
                end
      in
        f_memoed
      end
end

structure TreeMemoFibo = MemoedFibo(TreeDict(IntInfLt))

structure PoorAutoMemoFibo : FIBO =
struct
  structure TreeIntInfMemoizer = PoorMemoizer(TreeDict(IntInfLt))
  val fib = TreeIntInfMemoizer.memo Fibo.fib
end
