structure TreeFind : TREEFIND = 
struct
  datatype 'a ntree = Empty
                    | Node of 'a * 'a ntree list
  type 'a tree = 'a ntree

  exception NoSubtree
  
  (* fun : ('a -> bool) -> 'a tree -> 'a tree *)
  (* REQUIRES: true *)
  (* ENSURES: find p T evaluates to a subtree of T whose root satisfies p
   * and raises NoSubtree if no such subtree exists *)
  fun find (p : 'a -> bool) (Empty : 'a tree) : 'a tree = raise NoSubtree
    | find (p : 'a -> bool) (Node(x,[]) : 'a tree) : 'a tree = 
        if p x
          then Node(x,[])
          else raise NoSubtree
    | find (p : 'a -> bool) (Node(x,y::L) : 'a tree) : 'a tree =
        if p x
          then Node(x,y::L)
          else find p y handle NoSubtree => find p (Node(x,L))

end


structure TestTreeFind =
struct

(* Tests for find *)
val testTree1 = TreeFind.Node(1,[TreeFind.Node(2,[]),TreeFind.Node(3,[])])
val testTree2 = TreeFind.Node(1,[])

val (TreeFind.Node(1,[TreeFind.Node(2,[]),TreeFind.Node(3,[])])) =
    TreeFind.find (fn x => x = 1) testTree1 
val (TreeFind.Node(2,[])) = TreeFind.find (fn x => x = 2) testTree1 
val (TreeFind.Node(3,[])) = TreeFind.find (fn x => x = 3) testTree1
val (TreeFind.Node(1,[])) = TreeFind.find (fn x => x = 1) testTree2 

end
