structure Lazy : LAZY = 
struct 
  datatype lazylist = datatype LazyList.lazylist
  structure LazyList = LazyList

  (* Remove this when you're done to make sure you didn't miss anything *)
  exception Unimplemented

  (* Task 3.1 *)
  (* lazy_merge : ('a * 'a -> order) -> 'a lazylist ->
   * 'a lazylist -> 'a lazylist *)
  (* REQUIRES: L1 and L2 are cmp-sorted *)
  (* ENSURES: lazy merge cmp L1 L2 evaluates to a cmp-sorted lazy list
   * such that show n (lazy merge cmp L1 L2)  consists of the
   * n smallest elements from the set of items belonging to L1 or L2 *)
  fun lazy_merge cmp (L1 as Cons(a1, f1)) (L2 as Cons(a2, f2)) =
      case cmp(a1, a2) of
          GREATER => Cons(a2, (fn () => lazy_merge cmp L1 (f2())))
        | _ => Cons(a1, (fn () => lazy_merge cmp (f1()) L2))

  (* Task 3.2 *)
  (* combine : ('a * 'a -> order) -> 'a lazylist lazylist -> 'a lazylist *)
  (* REQUIRES: L is a laxy list of cmp-sorted lazy lists and the lazy list
   * consisting of the first elements of each lazy list in L is also
   * cmp-sorted *)
  (* ENSURES: combine L evaluates to a cmp-sorted lazy list such that
   * show n (combine cmp L) consists of the n smallest elements from the
   * set of items belonging to the lazy lists in L *)
  fun combine cmp (Cons(Cons(x, xs), R)) = 
      let
        val Cons(y, ys) = R()
      in
        Cons(x, (fn () => combine cmp (Cons((lazy_merge cmp (xs()) y), ys))))
      end

  (* Task 3.3 *)
  (* pairsHelper : (int * int) lazylist *)
  (* REQUIRES: true *)
  (* ENSURES: pairsHelper L evalutes to the lazy list of the first value
   * in each pair in L incremented by one *)
  fun pairsHelper (Cons((x,y),R)) =
      Cons((x+1,y), (fn () => pairsHelper (R())))
  
  
  (* pairs : (int * int) lazylist *)
  val pairs = combine
              (fn ((a1,a2),(b1,b2)) =>
               case (Int.compare(a1 + a2, b1 + b2),
                  Int.compare(a1, b1)) of
                   (LESS, _) => LESS  
                | (GREATER, _) => GREATER
                | (_, GREATER) => GREATER
                | (_, _) => EQUAL)
             (LazyList.iterate pairsHelper
              (LazyList.iterate (fn (x,y) => (x,y+1)) (0,0)))
      
end

structure LazyTests = 
struct
  open LazyList
  open Lazy

  (* Tests for lazy_merge *)
  val L1 = LazyList.iterate (fn x => x + 1) 0
  val L2 = LazyList.iterate (fn x => x + 2) 0
  val [0,0,1,2,2,3,4,4,5,6] =
  (show 10 (lazy_merge Int.compare L1 L2))
  
  (* Tests for combine *)
  fun combineTest (Cons(x,R)) =
      Cons(x+2, (fn () => combineTest (R())))
  val [0,1,2,2,3,3,4,4,4,5] =
  show 10 (combine Int.compare (LazyList.iterate combineTest
                                (LazyList.iterate (fn x => x+1) 0)))

  (* Tests for pairs *)
  val [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)] =
  show 10 pairs

end
