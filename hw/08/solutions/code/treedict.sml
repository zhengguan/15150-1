(* NOTE: Methodologies are very similar to those in fundict.sml *)

functor TreeDict (K : ORDERED) : DICT =
struct

  structure Key = K

  (* Invariant: BST *)
  datatype ('k, 'v) tree =
    Empty
  | Node of ('k, 'v) tree * ('k * 'v) * ('k, 'v) tree

  type 'v dict = (Key.t, 'v) tree

  (* ENSURES : empty is the empty dictionary expressed as a tree. *)
  val empty = Empty

  (* insert : 'v dict -> Key.t * 'v -> 'v dict *)
  (* REQUIRES: true *)
  (* ENSURES: insert d (k,v) returns the dictionary with (k,v)
   *          inserted into d (replacement if necessary) *)
  fun insert d (k, v) =
      case d of
          Empty => Node (empty, (k, v), empty)
        | Node (L, (k', v'), R) =>
          case Key.compare (k, k') of
              EQUAL => Node (L, (k, v), R)
            | LESS => Node (insert L (k, v), (k', v'), R)
            | GREATER => Node (L, (k', v'), insert R (k, v))

  (* lookup : 'v dict -> Key.t -> 'v option *)
  (* REQUIRES: true *)
  (* ENSURES: lookup d k returns SOME(v) if d contains a
   *          mapping from k to v and NONE otherwise *)
  fun lookup d k =
      case d of
          Empty => NONE
        | Node (L, (k', v'), R) =>
          case Key.compare (k, k') of
              EQUAL => SOME v'
            | LESS => lookup L k
            | GREATER => lookup R k

  (* rightmost : ('k * 'v) tree -> (('k * 'v) * ('k * 'v) tree) option *)
  (* REQUIRES: true *)
  (* ENSURES: rightmost T returns the rightmost element of T if it exists,
   *          along with the tree it is in, and NONE otherwise *)
  fun rightmost (T : ('k, 'v) tree) : (('k * 'v) * ('k, 'v) tree) option =
      case T of
          Empty => NONE
        | Node (L, (k, v), R) =>
          case rightmost R of
              NONE => SOME ((k, v), L)
            | SOME (p, R') => SOME (p, Node (L, (k, v), R'))

  (* remove : 'v dict -> Key.t -> 'v dict *)
  (* REQUIRES: true *)
  (* ENSURES: remove d k returns d' where d' is d with k and its
   *          value v removed *)
  fun remove d k =
      case d of
          Empty => Empty
        | Node (L, (k', v'), R) =>
          (case Key.compare (k, k') of
               EQUAL =>
               (case rightmost L of
                    NONE => R
                  | SOME ((kr, vr), L') => Node (L', (kr, vr), R))
             | LESS => Node (remove L k, (k', v'), R)
             | GREATER => Node (L, (k', v'), remove R k))

  (* map : ('v -> ''v) -> 'v dict -> ''v dict *)
  (* REQUIRES: f is total *)
  (* ENSURES: map f d returns d' where d' is d with all (k,v)
   *         mapped to (k,(f v)) *)
  fun map f d =
    case d of
      Empty => Empty
    | Node(L, (k, u), R) =>
      Node(map f L, (k, f u), map f R)

  (* filter : ('v -> bool) -> 'v dict -> 'v dict *)
  (* REQUIRES: p is total *)
  (* ENSURES: filter p d returns d' where d' is d with only (k,v)
   *          such that (p v) = true *)
  fun filter p d =
    case d of
      Empty => Empty
    | Node(L, (k, v), R) =>
      if p v then Node(filter p L, (k, v), filter p R)
      else filter p (remove d k)

end

(* So why do the tests look the same as in FunDict?
 * Well, we ARE using a functor, so the structure
 * we get back should still follow the exact same
 * specifications as given in the signature! *)
structure TestTreeDict =
struct

structure IntDict = TreeDict(IntOrder)
open IntDict

fun testTD () : bool =
    let
      val emp : int dict = empty
      val ins1 = insert empty (1,1)
      val ins2 = insert ins1 (2,2)

      val NONE = lookup emp 1
      val SOME(1) = lookup ins1 1
      val SOME(2) = lookup ins2 2
      val NONE = lookup ins2 3

      val testRem = remove ins2 1
      val NONE = lookup testRem 1
      val SOME(2) = lookup testRem 2

      val mapped = map (fn v => v+5) ins2
      val SOME(6) = lookup mapped 1
      val SOME(7) = lookup mapped 2

      val filtered = filter (fn v => (v mod 2) = 0) mapped
      val NONE = lookup filtered 2
      val SOME(6) = lookup filtered 1
    in
      true
    end

val true = testTD()

end
