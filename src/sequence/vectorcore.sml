structure VectorCore :> SEQUENCECORE =
struct
  type 'a seq = 'a vector

  exception Range of string

  val length = Vector.length

  fun nth i s =
      case (Int.compare (i,0), Int.compare(i, (length s) - 1))
       of (LESS,_) => raise (Range "nth called with a negative index")
        | (_,GREATER) => raise (Range "nth called with too large an index")
        | _ => Vector.sub (s,i)

  fun tabulate f n =
      case (Int.compare (n,0), Int.compare (n, Vector.maxLen))
       of (LESS,_) => raise Range "tabulate called with a negative length argument"
        | (_, GREATER) => raise Range "tabulate called with too large a length argument"
        | _ =>
            (* if the spec given in the Basis is correct, this tabulate
             * won't itself raise Size *)
            Vector.tabulate(n, f)
            handle Size => raise Range "tabulate applied to a function that raised Size"
                 | Range s => raise (Range ("tabulate applied to a function that raised Range with"
                                    ^ "\"" ^ s ^ "\""))

  fun filter f s =
      Vector.fromList
          (Vector.foldr (fn (e, xs) => case f e of true => e::xs | _ => xs) [] s)

  fun map f s = Vector.map f s
      handle (Range s) =>
             raise Range ("map called with a function that raised Range with \""
                   ^ s ^ "\"")

  fun reduce f b s = Vector.foldr f b s
      handle (Range s) =>
             raise Range ("reduce called with a function that raised Range with \""
                   ^ s ^ "\"")

  datatype 'a lview = Nil | Cons of 'a * 'a seq
  datatype 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  fun showl s =
      case length s
       of 0 => Nil
        | l => Cons(nth 0 s, tabulate (fn i => nth (i+1) s) (l-1))

  fun hidel Nil = Vector.fromList []
    | hidel (Cons (x,xs)) = Vector.concat [Vector.fromList [x], xs]

  fun showt s =
      case length s
       of 0 => Empty
        | 1 => Leaf (nth 0 s)
        | n =>
          let
            val mid = n div 2
          in
            Node (tabulate (fn i => nth i s) mid,
                  tabulate (fn x => nth (x + mid) s) (n - mid))
          end

  fun hidet Empty = Vector.fromList []
    | hidet (Leaf x) = Vector.fromList [x]
    | hidet (Node(s1,s2)) = Vector.concat [s1,s2]
end
