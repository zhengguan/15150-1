functor Stack (T : TYPEDEF) : STACK =
struct
  type stacktype = T.t
  type stack = stacktype list

  exception StackUnderflow
  (* empty : unit -> stack *)
  (* REQUIRES: true *)
  (* ENSURES: empty() returns an empty stack *)
  fun empty () : stack = []

  (* push : stack * stacktype -> stack *)
  (* REQUIRES: true *)
  (* ENSURES: push(S,elem) returns a stack with elem
   *          pushed onto S *)
  fun push (S : stack, elem : stacktype) = elem::S

  (* pop : stack -> stacktype * stack *)
  (* REQUIRES: pop is not empty *)
  (* ENSURES: pop L returns a tuple containing the first
   *          element e popped off of L and the remaining
   *          stack after popping e off of L; if L is
   *          empty, pop L raises a StackUnderflow exception *)
  fun pop (L : stack) : stacktype * stack = case L of
                [] => raise StackUnderflow
              | x::xs => (x, xs)

  (* append : stack * stack -> stack *)
  (* REQUIRES: true *)
  (* ENSURES: append(S1,S2) returns a stack with S1
   *          appended to S2 *)
  fun append (S1 : stack, S2 : stack) = S1 @ S2

  (* find : stack * (stacktype -> bool) -> stack option *)
  (* REQUIRES: true *)
  (* ENSURES: find (S,p) returns SOME(S') if there exists
   *          a largest substack S' of S where the first
   *          element x to be popped off of S' satisfies
   *          p x and NONE if no such element can be found *)
  fun find ([] : stack, p : stacktype -> bool) : stack option = NONE
    | find (x::xs, p) = if p x
    			then SOME(x::xs)
    			else find (xs, p)

  (* flip : stack -> stack *)
  (* REQUIRES: true *)
  (* ENSURES: flip S returns S', where S' is the
   *          stack S flipped upside down like a
   *          stack of pancakes *)
  fun flip (S : stack) : stack = List.rev(S)
end

structure TestStackImp =
struct

structure TTest : TYPEDEF =
struct
type t = int
end

structure TestStack : STACK = Stack(TTest)
open TestStack

fun testStackComplete () : bool =
    let
      val [] = empty()
      val [5] = push(empty(),5)
      val [1,2] = push(push(empty(),2),1)
      val (5,[]) = pop(push(empty(),5))
      val (1,[2]) = pop(push(push(empty(),2),1))
      val [5] = append(push(empty(),5),empty())
      val [] = append(empty(),empty())
      val [3,4] = append(push(empty(),3),push(empty(),4))
      val NONE = find (empty(),(fn x => true))
      val NONE = find (push(empty(),5), (fn x => (x mod 2 = 0)))
      val SOME([5]) = find (push(empty(),5), (fn x => (x=5)))
      val SOME([2,3]) = find (push(push(push(empty(),3),2),1), (fn x => (x=2)))
      val [] = flip(empty())
      val [1] = flip(push(empty(),1))
      val [3,2] = flip(push(push(empty(),3),2))
    in
      true
    end

val true = testStackComplete()

end
