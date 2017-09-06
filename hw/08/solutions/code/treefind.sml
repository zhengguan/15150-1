structure TreeFind =
struct
datatype 'a ntree = Empty
		  | Node of 'a * ('a ntree list)

exception NoSubtree
(* find : ('a -> bool) -> 'a ntree -> 'a ntree *)
(* REQUIRES : true *)
(* ENSURES : find p T evaluates to a subtree of T such that p is true
 * for the root element of T *)
fun find (p : 'a -> bool) (T : 'a ntree) : 'a ntree =
    case T of
      Empty => raise NoSubtree
    | Node(elem, L) =>
      if (p elem) : bool
      then T
      else (case L of
	      [] => raise NoSubtree
	    | x::xs =>
	      ((find p x):'a ntree ) handle NoSubtree =>
		  			    find p (Node(elem, xs)))
end

structure TestTreeFind =
struct

open TreeFind

val emp = Empty
val single = Node(1,[])
val line = Node(3,[Node(2,[Node(5,[Node(9,[])])])])
val multi1 = Node(5,[Node(2,[]),Node(4,[])])
val multi2 = Node(5,[Node(3,[Node(11,[])]),Node(7,[]),Node(1,[Node(9,[]),Node(6,[])])])

val odd = (fn x => (x mod 2) = 1)
val even = (fn x => (x mod 2) = 0)
val eqTo = (fn target => fn test => Int.compare(target,test) = EQUAL)

(* Testing Empty Tree *)
fun testEmpty () : bool =
    let
      val Empty = find odd emp handle _ => Empty
      val Node(~1,[]) = find (eqTo 0) emp handle _ => Node(~1,[])
    in
      true
    end

(* Testing Single *)
fun testSingle () : bool =
    let
      val Node(1,[]) = find odd single
      val Node(~1,[]) = find even single handle _ => Node(~1,[])
    in
      true
    end

(* Testing Line *)
fun testLine () : bool =
    let
      val Empty = find (eqTo 11) line handle _ => Empty
      val Node(9,[]) = find (eqTo 9) line
      val Node(5,[Node(9,[])]) = find (eqTo 5) line
      val Node(3,[Node(2,[Node(5,[Node(9,[])])])]) = find (eqTo 3) line
    in
      true
    end

(* Testing Multi *)
fun testMulti () : bool =
    let
      val Node(4,[]) = find (eqTo 4) multi1
      val Node(5,[Node(2,[]),Node(4,[])]) = find odd multi1
      val Node(3,[Node(11,[])]) = find (eqTo 3) multi2
      val Node(6,[]) = find even multi2
    in
      true
    end

val true = testEmpty()
val true = testSingle()
val true = testLine()
val true = testMulti()

end
