structure TreeSets : INTSET =
struct
  exception NotYetImplemented
  exception IntentionallyUnimplemented

  exception NotInSet

  datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  type set = int tree

  val empty = fn () => Empty

  fun find (n : int) (Empty : set) = false
    | find (n) (Node(L, x, R)) =
      (case Int.compare (n, x) of
        EQUAL => true
      | GREATER => find n R
      | LESS => find n L)


  fun insert (n : int) (Empty : set) = Node(Empty, n, Empty)
    | insert (n) (s2 as Node(L, x, R)) =
      case n = x of
        true => s2
      | false => if(n < x)
                 then Node(insert n L, x, R)
                 else Node(L, x, insert n R)

  (* delete : int -> set -> set
   * requires: true
   * ensures: raises IntentiallyUnimplemented
   *
   * You don't have to implement delete; though once you learn mutually recursive
   *   functions, you'll be able to.
   *
   * Luckily, you can write union, intersection, and difference without delete!
   *)
  fun delete (n : int) (s : set) = raise IntentionallyUnimplemented

  fun union (Empty : set) (s2 : set) = s2
    | union (Node(L, x, R)) (s2) =
      if (find x s2) then union R (union L s2)
      else union R (union L (insert x s2))

  fun intersection (Empty : set) (s2 : set) = Empty
    | intersection (s1) (Empty) = Empty
    | intersection (Node(L, x, R)) (s2) =
      if (find x s2)
      then insert x (union (intersection L s2) (intersection R s2))
      else (union (intersection L s2) (intersection R s2))

  fun difference (Empty : set) (s2 : set) = Empty
    | difference (s1: set) (Empty : set) = s1
    | difference (s1) (Node(L,x,R)) =
      if (find x s1)
      then union (difference s1 L) (difference s1 R)
      else insert x (union (difference s1 L) (difference s1 R))

end

structure TestTreeSets =
struct
  (* 'open' brings everything in the TreeSet namespace into this structure.
   *   We can now use 'insert' instead of 'TreeSets.insert'
   *)
  open TreeSets

  (* t = <5, 6> *)
  val t = empty()
  val t = insert 5 t
  val t = insert 6 t
  val true = find 5 t
  val true = find 6 t

  (* r = <7> *)
  val r = insert 7 (empty())
  val true = find 7 r

  (* s = <5> *)
  val s = insert 5 (empty())
  val true = find 5 s


  (* Test union *)
  val q = union r t
  val true = find 5 q
  val true = find 6 q
  val true = find 7 q
  val false = find 8 q

  (* Test intersection *)
  val i = intersection r t
  val false = find 6 i
  val false = find 5 i
  val false = find 7 i

  val i = intersection s t
  val true = find 5 i
  val false = find 6 i
  val false = find 7 i

  (* Test difference *)
  val d = difference r t
  val true = find 6 d
  val true = find 5 d
  val true = find 7 d

  val d = difference t s
  val true = find 5 d
  val true = find 6 d
  val false = find 7 d

end
