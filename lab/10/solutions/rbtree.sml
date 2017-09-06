structure RbTree=
struct

exception Unimplemented

(* A possible implementation of RBTs for keys as integers *)
type key = int
val compare : key * key -> order = Int.compare
datatype color = Red | Black
datatype 'v rbtree = Empty
  | Node of 'v rbtree * (color * (key * 'v)) * 'v rbtree

(* Task 3.1 *)
fun blackdepth Empty = 0
  | blackdepth (Node(l, (c, _), r)) = if (c=Black) then
      Int.max(blackdepth l, blackdepth r) + 1
    else Int.max(blackdepth l, blackdepth r)

fun isRBT Empty = true
  | isRBT (Node(Empty, _, Empty)) = true
  | isRBT (Node(l, (c, (k, _)), Empty)) =
    let val (Node(_, (lc, (lk, _)), _)) = l
    in if (c = Red andalso lc = Red) then false
       else (blackdepth(l) = 0)
    end
  | isRBT (Node(Empty, (c, (k, _)), r)) =
    let val (Node(_, (rc, (rk, _)), _)) = r
    in if (c = Red andalso rc = Red) then false
       else (blackdepth(r) = 0)
    end
  | isRBT (Node(l, (c, (k,_)), r)) =
    let val (Node(_, (lc, (lk, _)), _)) = l
        val (Node(_, (rc, (rk, _)), _)) = r
    in if (c = Red andalso (lc = Red orelse rc = Red)) then false
       else (blackdepth(l) = blackdepth(r) andalso
             isRBT(l) andalso isRBT(r))
    end

val e = Empty
val t1 = Node(e,(Red, (3, 3)),e)
val t2 = Node(t1,(Red, (4,10)), e)
val t3 = Node(e,(Red, (63,23)), t1)
val t4 = Node(t1,(Black, (10, 3289)), e)
val t5 = Node(e,(Black, (43,234)),e)
val t6 = Node(t5,(Red, (4,234)), e)
val t7 = Node(e,(Red, (16,234)), t5)
val t8 = Node(t4, (Black, (11,12)), t5)
val t9 = Node(t4, (Black, (11,12)), t7)
val true = isRBT(e)
val true = isRBT(t1)
val false = isRBT(t2) (* well-red *)
val false = isRBT(t3) (* ordering *)
val true = isRBT(t4)
val true = isRBT(t5)
val false = isRBT(t6) (* ordering *)
val false = isRBT(t7) (* black-depth *)
val true = isRBT(t8)
val false = isRBT(t9) (* black-depth *)

end
