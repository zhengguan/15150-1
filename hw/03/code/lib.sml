(* inteq : int * int -> bool
 * REQUIRES: true
 * ENSURES: inteq(x,y) is true iff x = y
 *)
fun inteq (l1 : int , l2 : int) : bool =
    case Int.compare (l1,l2)
     of EQUAL => true
      | _ => false

val false = inteq(~7, 7)
val false =inteq(~7, 0)
val true = inteq(~7, ~7)


(* addToEach : int list * int -> int list
 * REQUiRES: true
 * ENSURES: adds n to each element of the list l
 *)
fun addToEach (l : int list, n : int) : int list =
    case l of
        nil => nil
      | x::xs => x + n :: addToEach (xs, n)

val nil = addToEach (nil, 7)
val 4::5::6::nil = addToEach (1::2::3::nil, 3)
val 3::2::1::nil = addToEach (6::5::4::nil, ~3)

