(* Lazy programming with nats and lists *)
structure LazyProgrammer =
struct

datatype 'a lazylist = Nil
                     | Cons of 'a * (unit -> 'a lazylist)

datatype lazynat = Zero | Succ of unit -> lazynat

(* Examples from the handout - feel free to reuse in your tests. *)
(* Returns an infinite list of 0's *)
fun zeros () = Cons (0, zeros)

(* decreasing : int -> int lazylist
   REQUIRES: n >= 0
   ENSURES: decreasing n is a finite lazy list,
            consisting of the integers n, ..., 1,  if n > 0;
                                         and NIL,  if n=0.
   If n < 0, the function will still return a lazy list, now infinite,
   of the form     n, n-1, n-2, ...
*)
fun decreasing 0 = Nil
  | decreasing n = Cons(n, fn () => decreasing(n-1))

(* take : 'a lazylist * int -> 'a list
   ENSURES: take(L, n) returns the first n elements of L, now as a regular
            list, unless L has fewer than n elements, in which case take raises Fail.
*)
fun take (L : 'a lazylist, 0 : int) : 'a list = nil
  | take (Nil, _) = raise Fail "tried to take from a Nil lazylist"
  | take (Cons(x, f), n) = x::take(f(),n-1)


(* DOCUMENTATION GOES HERE *)
fun lazy_append (L1 : 'a lazylist, L2 : 'a lazylist) : 'a lazylist =
    case (L1,L2) of 
        (Cons(a, f), L4) => Cons(a, fn () => lazy_append(f(), L4))
      | (Nil, L4) => L4


(* returns the lazy representation of infinity *)
fun infinity () = Succ infinity

(* This means 2 + infinity, which is still infinity *)
val silly_infinity = Succ (fn () => Succ infinity)

(* Lazy representation of 2 *)
val two = Succ (fn () => Succ (fn () => Zero))

(* DOCUMENTATION GOES HERE *)
fun lazy_length (L : 'a lazylist) : lazynat =
    case L of 
        Cons(a, f) => Succ(fn() => lazy_length(f()))
      | Nil => Zero

(* Testing lazy_length is a little tricky unless you write lazy_cmp
 * below. Here's some testing code to get you started: *)
fun minus1 (n : lazynat) : lazynat =
    case n of
      Zero => raise Fail "Tried to created negative nat"
    | Succ f => f ()

(*
val two_list = Cons (4, fn () => Cons (5, fn () => Nil))
val Zero = minus1 (minus1 (lazy_length two_list))
*)


(* BONUS *)
(* DOCUMENTATION GOES HERE *)
fun lazy_cmp (n : lazynat) (m : int) : order =
    case (n,m) of
        (Zero, 0) => EQUAL
      | (Zero, _) => LESS
      | (_, 0) => GREATER
      | (Succ(f), m) => (lazy_cmp (f()) (m-1)) 
      

(* DOCUMENTATION GOES HERE *)
fun int_to_lazy (0 : int) : lazynat = Zero
  | int_to_lazy (n : int) : lazynat = Succ(fn() => int_to_lazy(n-1))
end
