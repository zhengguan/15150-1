structure ListSets : INTSET =
struct
  exception NotYetImplemented
  exception NotInSet

  type set = int list

  val empty = fn () => []

  fun find (n : int) (s : set) =
    case s of
      [] => false
    | x :: l =>  (x = n) orelse (find n l)

  (* in this version, we allow you to keep duplicates when inserting *)
  fun insert (n : int) (s : set) = n :: s

  (* you must delete all duplicates in this *)
  (* also should handle the case in which n is not in the set *)
  fun delete (n : int) (s : set) =
    case find n s of
      false => raise NotInSet
    | _ => List.filter (fn x => not (x = n)) s

  (* union is also allowed to have duplicates in it *)
  fun union (s1 : set) (s2 : set) =
     s1@s2

  (* all elements in s1 and s2 *)
  fun intersection (s1 : set) (s2 : set) =
    case (s1, s2) of
      ([],_) => []
    | (_,[]) => []
    | (_,_) => List.filter (fn x => find x s2) s1


  (* s1 - s2 *)
  (* Note - we're removing all instances of the duplicate *)
  fun difference (s1 : set) (s2 : set) =
    case (s1, s2) of
      ([], _) => []
    | (_,[]) => s1
    | (_,_) => List.filter (fn x => not(find x s2)) s1

end

structure TestListSets =
struct
  (* Note that we have to use 'ListSets.find' rather than 'find',
   *  since we're in a different structure.
   *)
  val true = ListSets.find 1 [1,2,3]
  val [1,1,2] = ListSets.insert 1 [1,2]
  val [2] = ListSets.delete 1 [1,1,2]
  val [1,2,3,4] = ListSets.union [1,2] [3,4]
  val [1,2] = ListSets.intersection [1,2,3,4] [5,6,1,2]
  val [1,2] = ListSets.difference [1,2,3,4] [3,4]
end
