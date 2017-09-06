structure ListSets : INTSET =
struct
  exception NotYetImplemented
  exception NotInSet

  type set = int list

  val empty = fn () => []

  fun find (n : int) (s : set) = foldl (fn (x,y) => x orelse y) false (map (fn z => n = z) s)

  fun insert (n : int) (s : set) = n::s

  fun delete (n : int) (s : set) = List.filter (fn x => x <> n) s

  (* union, intersection, and difference must use higher-order functions *)
  fun union (s1 : set) (s2 : set) = s1 @ s2

  fun intersection (s1 : set) (s2 : set) = List.filter (fn x => find x s2) s1

  fun difference (s1 : set) (s2 : set) = List.filter (fn x => not(find x s2)) s1

end

structure TestListSets =
struct
  (* Note that we have to use 'ListSets.find' rather than 'find',
   *  since we're in a different structure.
   *)
  val emptySet = ListSets.empty()
  val oneFive = ListSets.insert 5 emptySet
  val true = ListSets.find 5 oneFive

  (* Your tests here! *)

  (* Tests for find *)
  val true = ListSets.find 3 [1,2,3]
  val true = ListSets.find 4 [4,5,4]
  val false = ListSets.find 4 [3,2,1]
  val false = ListSets.find 1 []

  (* Tests for insert *)
  val [1,2,3] = ListSets.insert 1 [2,3]
  val [1,1,2,3] = ListSets.insert 1 [1,2,3]
  val [4] = ListSets.insert 4 []

  (* Tests for delete *)
  val [1,2] = ListSets.delete 3 [1,2,3]
  val [1,2,3] = ListSets.delete 4 [1,2,3]
  val [2,3] =  ListSets.delete 1 [1,2,3,1]
  val [] = ListSets.delete 1 []

  (* Tests for union *)
  val [1,2,3,4,5] = ListSets.union [1,2,3] [4,5]
  val [1,2,3,3,4,5] = ListSets.union [1,2,3] [3,4,5]    
  val [1,2,3] = ListSets.union [1,2,3] []

  (* Tests for intersection *)
  val [] = ListSets.intersection [1,2,3] [4,5]
  val [1,2,3] = ListSets.intersection [1,2,3] [1,2,3]
  val [3] = ListSets.intersection [1,2,3] [3,4,5]    
  val [] = ListSets.intersection [1,2,3] [] 

  (* Tests for difference *)
  val [1,2,3] = ListSets.difference [1,2,3] [4,5]
  val [] = ListSets.difference [1,2,3] [1,2,3]
  val [1,2] = ListSets.difference [1,2,3] [3,4,5]    
  val [1,2,3] = ListSets.difference [1,2,3] []
    
end
