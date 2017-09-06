use "lib.sml";

(* ---------------------------------------------------------------------- *)
(* Section 3 - Bases *)
(* Task 3.1 *)
(* toInt : int -> int list -> int *)
(* REQUIRES: base > 1 *)
(* ENSURES: toInt(b)(L) evaluates to n where L is the representation of n in base b *)
fun toInt (base : int) ([] : int list) : int = 0
  | toInt (base : int) (x::digits : int list) = (toInt(base) digits) * base + x
  
(* Tests for toInt *)
val 2 = toInt(2) ([0,1])
val 11 = toInt(3) ([2,0,1])
val 27 = toInt(4) ([3,2,1])
val 0 = toInt(2) ([])

(* Task 3.2 *)
(* toBase int -> int -> int list *)
(* REQUIRES: base > 1, n>=0 *)
(* ENSURES: toBase(b)(n) evaluates to the representation of n in base b *)
fun toBase (base : int) (0 : int) : int list = []
  | toBase (base : int) (n : int) : int list = (n mod base)::(toBase(base) (n div base))

(* Tests for toBase *)
val [0,1] = toBase(2) (2)
val [2,0,1] = toBase(3) (11)
val [3,2,1] = toBase(4) (27)
val [] = toBase(2) (0)

(* Task 3.3 *)
(* convert : int * int -> int list -> int list *)
(* REQUIRES: b1 > 1, b2 < 1 *)
(* ENSURES: toInt b2 (convert(b1, b2) L) = toInt b1 L *)
fun convert (base1 : int, base2 : int) (digits : int list) : int list = toBase(base2) (toInt(base1) digits)

(* Tests for convert *)
val [6,1] = convert(10, 8) ([4,1])
val [1,0,1] = convert(2, 5) ([0,1,0,1,1,0])
val [5,4,1] = convert(3, 6) ([2,0,1,2])
val [0,2,2,2] = convert(5, 3) ([3,5,2])


(* ---------------------------------------------------------------------- *)
(* Section 4 - Polymorphism, HOFs, Options *)
(* Task 4.1 *)
(* dotProduct : real list * real list - > real *)
(* REQUIRES: length(a) = length(b) *)
(* ENSURES: dotProduct(a,b) evaluates to the dot product of a and b *)
fun dotProduct (a : real list, b : real list) : real = foldl(op + ) 0.0 (map(op * ) (zip(a, b)))

(* Tests for dotProduct *)
val true = Real.==(dotProduct([1.0,2.0,3.0], [5.0,2.0,2.0]), 15.0)
val true = Real.==(dotProduct([1.0,3.0], [0.0,2.0]), 6.0)
val true = Real.==(dotProduct([], []), 0.0)

(* Task 4.2 *)
(* magnitudeOfVector : real list -> real *)
(* REQUIRES: true *)
(* ENSURES: magnitudeOfVector(a) evalutes to the magnitude of the vector a *)
fun magnitudeOfVector (a : real list) : real = Math.sqrt(foldl(op + ) 0.0 (map(fn n => n * n) a))
  
(* Tests for magnitudeOfVector *)
val true = Real.==(magnitudeOfVector([1.0,3.0,4.0]), Math.sqrt(26.0))
val true = Real.==(magnitudeOfVector([2.0,~2.0,5.0]), Math.sqrt(33.0))
val true = Real.==(magnitudeOfVector([7.0]), 7.0)
val true = Real.==(magnitudeOfVector([0.0,3.0]), 3.0)
val true = Real.==(magnitudeOfVector([0.0,0.0,0.0]), 0.0)
val true = Real.==(magnitudeOfVector([]), 0.0)

(* Task 4.3 *)
(* angleBetweenVectors : real list * real list -> real *)
(* REQUIRES: a and b are non-empty *)
(* ENSURES: angleBetweenVectors(a,b) evaluates to the angle between the vectors a and b *)
fun angleBetweenVectors (a : real list, b : real list) : real = Math.acos(dotProduct(a, b)/(magnitudeOfVector(a) * magnitudeOfVector(b)))

(* Tests for angleBetweenVectors *)
val true = Real.==(angleBetweenVectors([1.0,0.0],[0.0,1.0]), Math.acos(0.0))
val true = Real.==(angleBetweenVectors([0.0,2.0],[2.0,0.0]), Math.acos(0.0))
val true = Real.==(angleBetweenVectors([~3.0, 1.0],[1.0,3.0]), Math.acos(0.0))

(* Task 4.4 *)
(* extract : ('a -> bool) * 'a list -> ('a * 'a list) option *)
(* REQUIRES: true *)
(* ENSURES: extract(p, l) evaluates to SOME(p, l') if there is an x in l such that p(x)=true and l' is l without x and evaluates to NONE otherwise *)
fun extract (p : 'a -> bool, [] : 'a list) : ('a * 'a list) option = NONE
  | extract (p : 'a -> bool, x::l : 'a list) : ('a * 'a list) option =
    case extract(p, l) of
        SOME (y, r) => SOME (y, x::r)
      | NONE => 
          if p(x)
            then SOME (x, l)
            else NONE
            
(* Tests for extract *)
val SOME (3, [2,4]) = extract(fn x => x mod 2 = 1, [2,3,4])
val NONE = extract(fn x => x mod 2 = 1, [2,4,6])
val SOME (6, [2,4]) = extract(fn x => x mod 2 = 0, [2,4,6])
val NONE = extract(fn x => x mod 2 = 0, [])
val SOME ("b", ["aaa", "bca"]) = extract(fn x => String.size x < 2 , ["aaa","b","bca"])


(* ---------------------------------------------------------------------- *)
(* Section 5 - Blocks World *)
(* Task 5.1 *)
(* extractMany : (’a * ’a -> bool * ’a list * ’a list) -> (’a list) option *)
(* REQUIRES: true *)
(* ENSURES: extractMany(eq,toExtract,from) removes elements in toExtract from from if the elements in toExtract are in from and evaluates to NONE otherwise *)
fun extractMany (eq : 'a * 'a -> bool,
                 [] : 'a list, from : 'a list) : ('a list) option = SOME from
  | extractMany (eq : 'a * 'a -> bool,
                 toExtract : 'a list, [] : 'a list) : ('a list) option = NONE
  | extractMany (eq : 'a * 'a -> bool,
                 x::l : 'a list, from : 'a list) : ('a list) option = 
                  case extract(fn a => eq(a, x), from) of
                      SOME (y, r) => extractMany(eq, l, r)
                    | NONE => NONE
                    
(* Tests for extractMany *)
val SOME [2,3,3,4] = extractMany(fn(x,y) => x=y, [2,1,2], [1,2,3,3,2,4,2])
val NONE = extractMany(fn(x,y) => x=y, [2,2], [2])
val SOME [3] = extractMany(fn(x,y) => x=y, [], [3])
val NONE = extractMany(fn(x,y) => x=y, [4], [])


(* Task 5.2 *)
datatype block = A
               | B
               | C

datatype move = PickupTable of block
              | PutTable of block
              | PickupBlock of block * block
              | PutBlock of block * block

datatype fact = Free of block
              | OnBlock of block * block
              | OnTable of block
              | HandEmpty
              | HandHold of block

type state = fact list

(* Task 5.3 *)
val initial : state = [HandEmpty, OnTable(A), OnTable(B), OnTable(C), Free(A), Free(B), Free(C)]

(* instantiates extractMany with equality for your fact datatype *)
fun extractManyFacts (toConsume : fact list, s : state) : state option =
  extractMany (fn (x : fact, y : fact) => x = y, toConsume, s)

(* Task 5.4 *)
(* consumeAndAdd : (state * fact list * fact list) -> state option *)
(* REQUIRES: true *)
(* ENSURES: extractMany(s,before,after) removes elements in before from s and adds elements from after to s if the elements in before are in s and evaluates to NONE otherwise *)
fun consumeAndAdd (s : state, bef : fact list, aft : fact list) : state option =
      case extractManyFacts(bef, s) of
          SOME l => SOME (l @ aft)
        | NONE => NONE

(* Tests for consumeAndAdd *)
val SOME [HandEmpty, OnTable(A)] = consumeAndAdd([Free(A), HandEmpty], [Free(A)], [OnTable(A)])
val NONE = consumeAndAdd([Free(A), HandEmpty], [Free(B)], [OnTable(A)])
val SOME [Free(A), HandEmpty, OnTable(A)] = consumeAndAdd([Free(A), HandEmpty], [], [OnTable(A)])
val SOME [HandEmpty] = consumeAndAdd([Free(A), HandEmpty], [Free(A)], [])
val SOME [OnTable(A)] = consumeAndAdd([], [], [OnTable(A)])
val NONE = consumeAndAdd([], [Free(A)], [])
val SOME [] = consumeAndAdd([], [], [])

(* Task 5.5 *)
(* step : (move * state) -> state option *)
(* REQUIRES: true *)
(* ENSURES: step(m, s) applies the move m to the state s if the before facts hold and evaluates to NONE otherwise *)
fun step (m : move, s : state) : state option =
      case m of
          PickupTable(a) => consumeAndAdd(s, [Free(a), OnTable(a), HandEmpty], [HandHold(a)])
        | PutTable(a) => consumeAndAdd(s, [HandHold(a)], [HandEmpty, OnTable(a), Free(a)])
        | PickupBlock(a,b) => consumeAndAdd(s, [Free(a), OnBlock(a,b), HandEmpty], [Free(B), HandHold(A)])
        | PutBlock(a,b) => consumeAndAdd(s, [HandHold(a), Free(b)], [Free(a), HandEmpty, OnBlock(a,b)])
        
(* Tests for step *)
val SOME [Free(B), HandHold(A)] = step(PickupTable(A), [Free(A), OnTable(A), HandEmpty, Free(B)])
val NONE = step(PutTable(A), [Free(B)])
val NONE = step(PickupBlock(A,B), [])


(* ---------------------------------------------------------------------- *)
(* shrubMap : (’a -> ’b) -> ’a shrub -> ’b shrub *)
(* REUQIRES: f is a total function *)
(* ENSUERS: shrubMap(f)(s) evaluates to a a shrub with f applies to evert leaf *)
(* Task 7.1 *)
fun shrubMap (f : 'a -> 'b) (Leaf(a) : 'a shrub) : 'b shrub = Leaf(f(a))
  | shrubMap (f : 'a -> 'b) (Branch(a, b) : 'a shrub) : 'b shrub = Branch((shrubMap(f) a), (shrubMap(f) b))

(* Tests for shrubMap *)
val Branch(Leaf(4), Leaf(6)) = shrubMap(fn x => 2 * x) (Branch(Leaf(2), Leaf(3)))
val Branch(Leaf(3), Leaf(3)) = shrubMap(fn x => x + 3) (Branch(Leaf(0), Leaf(0)))
val Branch(Leaf(16), Leaf(0)) = shrubMap(fn x => x * x) (Branch(Leaf(4), Leaf(0)))
val Leaf(2) = shrubMap(fn x => x) (Leaf(2))

(* Task 7.4 *)
(* shrubCombine : (’a * ’a -> ’a) -> ’a -> ’a shrub -> ’a *)
(* REQUIRES:  f is a total associative function *)
(* ENSURES: shrubCombine(f)(i)(s) returns the result of recursively combining the shrub with f where i is the identity *)
fun shrubCombine (f : 'a * 'a -> 'a) (i : 'a) (Leaf(a) : 'a shrub) : 'a = f(a, i)
  | shrubCombine (f : 'a * 'a -> 'a) (i : 'a) (Branch(a, b) : 'a shrub) : 'a = f((shrubCombine(f) i a), (shrubCombine(f) i b))
  
(* Tests for shrubCombine *)
val 5 = shrubCombine(op + ) 0 (Branch(Leaf(1), Leaf(4)))
val 60 = shrubCombine(op * ) 1 (Branch(Leaf(6), Leaf(10)))
val 9 = shrubCombine(op + ) 2 (Branch(Leaf(5), Leaf(0)))
val 54 = shrubCombine(op * ) 3 (Branch(Leaf(2), Leaf(3)))
val 2 = shrubCombine(op * ) 1 (Leaf(2))
