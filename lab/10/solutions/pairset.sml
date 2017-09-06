functor PairSet (PE: PAIR_OF_EQUAL) : SET =
struct
  
  structure CartEqual : EQUAL =
  struct
    type t = PE.E1.t*PE.E2.t

    fun equal ((a1,b1) : t, (a2,b2) : t) = 
      PE.E1.equal(a1,a2) andalso PE.E2.equal(b1,b2)
  end

  structure CartDict = Dict(CartEqual)
  structure CartSet = DictSet(CartDict)

  open CartSet

end
