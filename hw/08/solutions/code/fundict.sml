(********** TASK 4.1 **********)
functor FunDict (K : ORDERED) : DICT =
struct
  structure Key = K

  datatype 'v func = Func of (Key.t -> 'v option)

  type 'v dict = 'v func

  (* ENSURES : empty is a dictionary that contains no mappings *)
  val empty = Func (fn _ => NONE)

  (* insert : 'v dict -> Key.t * 'v -> 'v dict *)
  (* REQUIRES : true *)
  (* ENSURES : insert f (k,v) inserts v into f at key k *)
  fun insert (Func f) (k, v) =
    Func
    (fn k' =>
      case Key.compare (k, k') of
        EQUAL => SOME v
      | _ => f k')

  (* lookup : 'v dict -> Key.t -> 'v option *)
  (* REQUIRES: true *)
  (* ENSURES: lookup f k finds the value of k in dictionary f*)
  fun lookup (Func f) k = f k

  (* remove : 'v dict -> Key.t -> 'v dict *)
  (* REQUIRES: true *)
  (* ENSURES: remove f k removes k and its value from f*)
  fun remove (Func f) k =
    Func
    (fn k' =>
      case Key.compare (k,k') of
        EQUAL => NONE
      | _ => f k')

  (* map : ('v -> ''v) -> 'v dict -> ''v dict *)
  (* REQUIRES : g is total *)
  (* ENSURES: map g d maps a function g over all values in d *)
  fun map g d =
    Func
    (fn k' =>
      case (lookup d k') of
        SOME x => SOME (g x)
      | _ => NONE)

  (* filter : ('v -> bool) -> 'v dict -> 'v dict *)
  (* REQUIRES: p is total *)
  (* ENSURES: filter p d returns a dictionary of all values in d satisfying
   * the predicate p. *)
  fun filter p d =
    Func
    (fn k' =>
      case (lookup d k') of
        SOME x =>
          (case (p x) of
             true => SOME x
           | false => NONE)
      | _ => NONE)
end

structure TestFunDict =
struct

structure IntDict = FunDict(IntOrder)
open IntDict

fun testFD () : bool =
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

val true = testFD()

end
