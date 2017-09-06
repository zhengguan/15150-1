functor MakeScalar(SC : SCALARCORE) : SCALAR =
struct
  open SC
  fun gte (x : scalar, y : scalar) : bool=
      case compare(x,y)
       of LESS => false
        | _ => true

  fun lte (x : scalar, y : scalar) : bool =
      case compare(x,y)
       of GREATER => false
        | _ => true

  fun min (x : scalar, y : scalar) : scalar =
      case compare(x,y)
       of LESS => x
        | _ => y

  fun max (x : scalar, y : scalar) : scalar=
      case compare(x,y)
       of GREATER => x
        | _ => y

  fun eq (x : scalar, y : scalar) : bool =
      case compare(x,y)
       of EQUAL => true
        | _ => false

  fun fromInt x = fromRatio (x,1)
  val zero = fromInt 0
  val one = fromInt 1

  (* simple and slow pow: assume exp : nat, exp many mults. *)
  fun pow (base : scalar, exp : int) : scalar =
      case exp
       of 0 => one
        | _ => times(base, pow(base, exp - 1))

  fun invert (x : scalar) = divide(one, x)

  fun negate (x : scalar) = times(x, fromInt(~1))
end
