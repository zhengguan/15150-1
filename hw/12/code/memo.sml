(* Task 4.1 *)
functor MemoedFibo (D : DICT where type Key.t = IntInf.int) : FIBO =
struct

  val fibDict : IntInf.int D.dict ref = ref D.empty;
  val _ = fibDict := D.insert (!fibDict) (0,0)
  val _ = fibDict := D.insert (!fibDict) (1,1)

  (* fib : IntInf.int -> IntInf.int *)
  (* REQUIRES: n>=0 *)
  (* ENSURES: fib n evaluates to the nth Fibonacci number *)
  fun fib (n : IntInf.int) : IntInf.int =
      case (D.lookup (!fibDict) n) of
          SOME(x) => x
        | _ => let
                    val x = fib(n-2) + fib(n-1)
                    val _ = fibDict := D.insert (!fibDict) (n,x)
                  in
                    x
                  end

end

(* Task 4.2 *)
(* If the result is not memoized, this code calls f to get the result to
 * memoize.  However it does not call the memoized version of f.  This makes
 * the code much slower because any recursive calls in f will not check to see
 * if their result is memoized. *)

(* Task 4.3 *)
functor Memoizer (D : DICT) : MEMOIZER =
struct
  structure D = D

  (* memo : ((D.Key.t -> 'a) -> (D.Key.t -> 'a)) -> (D.Key.t -> 'a) *)
  (* REQUIRES: true *)
  (* ENSURES: memo m evalutes to a memoized version of m *)
  fun memo (m : (D.Key.t -> 'a) -> (D.Key.t -> 'a)) : D.Key.t -> 'a =
      let
        val hist : 'a D.dict ref = ref D.empty

        fun m_memoed x =
            case D.lookup (!hist) x of
                SOME(b) => b
              | NONE =>
                let
                  val res = m m_memoed x
                  val _ = (hist := D.insert (!hist) (x,res))
                in
                  res
                end
     in
       m_memoed
     end
      
end

(* Task 4.4 *)
structure AutoMemoedFibo : FIBO =
struct

  structure TreeIntInfMemoizer = Memoizer(TreeDict(IntInfLt))  
  
  (* fibHelper : (IntInf.int -> IntInf.int) -> IntInf.int -> IntInf.int *)
  (* REQUIRES: n>=0 *)
  (* ENSURES: fibHelper n evaluates to the nth Fibonacci number *)
  fun fibHelper (m : IntInf.int -> IntInf.int) (n : IntInf.int) : IntInf.int =
      case n of
          0 => 0
        | 1 => 1
        | _ => m(n-2) + m(n-1)

  val fib = TreeIntInfMemoizer.memo fibHelper
  
end


structure TestAutoMemoedFibo =
struct
  open AutoMemoedFibo
  
  (* Tests for AutoMemoedFibo *)
  val 0 = fib 0
  val 1 = fib 1
  val 5 = fib 5
  val 6765 = fib 20
  val 12586269025 = fib 50
  val 280571172992510140037611932413038677189525 = fib 200
  
end
