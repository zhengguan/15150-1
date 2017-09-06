datatype 'a shrub = Leaf of 'a
                  | Branch of 'a shrub * 'a shrub

(* ---------------------------------------------------------------------- *)
(* Other provided functions *)

(* Purpose: Returns SOME of the nth (starting from index 0) element of the
 * argument list if the list has at least n+1 elements.  Otherwise, the result
 * is NONE.
 *)
fun nth (l : 'a list, n : int) : 'a option =
    case l of
        nil => NONE
      | x::xs => (case n of
                      0 => SOME x
                    | _ => nth (xs, n-1))

(* Purpose: Converts an argument list into a function that maps a natural
 * number int to the element of the list in that position if there is one.
 * Otherwise, the function maps the int to the value x.
 *)
fun listToFun (x : 'a, l : 'a list) : int -> 'a =
    fn y => case nth (l, y) of NONE => x | SOME x' => x'

fun width [] = 0
  | width (l::_) = length l

val height = List.length

(* Purpose : Hiding ListPair library *)

fun zip (a,b) = ListPair.zip (a,b)
