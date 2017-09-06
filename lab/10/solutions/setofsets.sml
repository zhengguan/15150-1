functor SetOfSets (S : SET) : SET = 
struct
    structure SetEqual : EQUAL =
     struct
       type t = S.set 

       fun equal (A : t,B : t) = 
         S.void (S.union (S.difference A B) (S.difference B A))
     end

  structure SetDict = Dict(SetEqual)

     structure Power = DictSet(SetDict)

     open Power
end
