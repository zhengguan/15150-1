functor DeriveSequence (S : SEQUENCECORE) :> SEQUENCE =
struct
  open S

  fun mapreduce f b c s = reduce c b (map f s)
      handle (Range s) =>
             raise Range ("mapreduce called with a function that raised Range with \""
                   ^ s ^ "\"")

  fun toString eltToString s =
      let
        fun f (NONE, x) = x
          | f (x, NONE) = x
          | f (SOME x, SOME y) = SOME (x ^ ", " ^ y)
      in
        "[" ^
         (case mapreduce (SOME o eltToString) NONE f s
           of NONE => "" | SOME s => s)
         ^ "]"
      end

  fun repeat n x = tabulate (fn _ => x) n

  fun zip (sa, sb) = tabulate (fn n => (nth n sa, nth n sb))
                              (Int.min(length sa, length sb))

  fun append s1 s2 = hidet (Node (s1, s2))

  fun empty () : 'a seq = hidel Nil

  fun singleton x = hidet (Leaf x)

  (* FIXME: implement using tabulate for better work/span? *)
  fun split i s =
      case i of 0 => (empty() , s)
              | _ => (* i > 0 *)
          (case (i,showt s) of
                (_,Empty) => raise Range "split can't get more than 0 elements of Empty"
              | (1, Leaf x) => (singleton x, empty ())
              | (_, Leaf _) => raise Range "split can't get more than 1 element of a Leaf"
              | (_, Node (l,r)) =>
                    let val ls = length l
                    in case Int.compare (i, ls)
                        of LESS => let val (ll, lr) = split i l
                                   in (ll, append lr r)
                                   end
                      | EQUAL => (l, r)
                      | GREATER => let val (rl, rr) = split (i - ls) r
                                   in (append l rl, rr)
                                   end
                    end)

  (* FIXME: implement directly for O(i) work ? *)
  fun take i s = (fn (x,_) => x) (split i s)
  fun drop i s = (fn (_,y) => y) (split i s)

  fun cons x s = hidel (Cons (x,s))

  (* FIXME: implement with tabulate instead for better work/span? *)
  fun flatten ss = reduce (fn (x,y) => append x y) (empty ()) ss
end
