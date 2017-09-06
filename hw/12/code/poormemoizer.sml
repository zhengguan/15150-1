functor PoorMemoizer (D : DICT) : POORMEMOIZER =
struct
  structure D = D

  fun memo (f : D.Key.t -> 'a) : D.Key.t -> 'a =
      let
        val hist : 'a D.dict ref = ref D.empty

        fun f_memoed x =
            case D.lookup (!hist) x
             of SOME(b) => b
              | NONE =>
                let
                  val res = f x
                  val _ = (hist := D.insert (!hist) (x,res))
                in
                  res
                end
      in
        f_memoed
      end
end
