functor LEOrder (X : sig
                         type t
                         val le : t * t -> bool
                     end) : ORDERED =
struct
    type t = X.t
    fun compare(x,y) =
        case (X.le(x,y), X.le(y,x)) of
            (true,true) => EQUAL
          | (true,false) => LESS
          | (false,true) => GREATER
          | (false,false) => raise Fail "LEOrder: no relationship"
end

functor FlipOrder(O : ORDERED) : ORDERED =
struct
    type t = O.t
    fun compare p =
        case O.compare p of
          EQUAL => EQUAL
        | LESS => GREATER
        | GREATER => LESS
end

(* order - * O by the O part, ignoring the first components *)
functor PairSecondOrder (X : sig
                                 type left
                                 structure Right : ORDERED
                             end) : ORDERED =
struct
    type t = X.left * X.Right.t
    fun compare ((_ , x) , (_ , y)) =
        X.Right.compare (x,y)
end

functor PairOrder (A : sig
                           structure O1 : ORDERED
                           structure O2 : ORDERED
                       end) : ORDERED =
struct

    type t = A.O1.t * A.O2.t

    fun compare ((x,y), (x',y')) =
        case A.O1.compare(x,x') of
            LESS => LESS
          | GREATER => GREATER
          | EQUAL => A.O2.compare(y,y')

end

functor ListOrder (O : ORDERED) : ORDERED =
struct

    type t = O.t list

    fun compare (l1, l2) =
        case (l1,l2) of
            ([],[]) => EQUAL
          | ([],_::_) => LESS
          | (_::_,[]) => GREATER
          | (x::xs,y::ys) => (case O.compare(x,y) of
                                  LESS => LESS
                                | GREATER => GREATER
                                | EQUAL => compare(xs,ys))

end

signature ORDERED_UTILS =
sig
    include ORDERED

    (* these return the first argument when the two are EQUAL *)
    val min : t * t -> t
    val max : t * t -> t

    val le : t * t -> bool
    val ge : t * t -> bool
    val lt : t * t -> bool
    val gt : t * t -> bool
    val eq : t * t -> bool
end

(* derive max and min for any ordered type *)
functor OrderUtils (O : ORDERED) : ORDERED_UTILS =
struct
    open O

    fun min (x,y) =
        case O.compare(x,y)
         of GREATER => y
          | _ => x

    fun max (x,y) =
        case O.compare(x,y)
         of LESS => y
          | _ => x

    fun le (x,y) =
        case O.compare(x,y)
         of LESS => true
          | EQUAL => true
          | GREATER => false

    fun lt (x,y) =
        case O.compare(x,y)
         of LESS => true
          | _ => false

    fun ge (x,y) =
        case O.compare(x,y)
         of GREATER => true
          | EQUAL => true
          | LESS => false

    fun gt (x,y) =
        case O.compare(x,y)
         of GREATER => true
          | _ => false

    fun eq (x,y) =
        case O.compare(x,y)
         of EQUAL => true
          | _ => false
end
