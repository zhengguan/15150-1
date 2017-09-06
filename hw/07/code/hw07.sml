
(* Remove this when you're done to make sure you didn't miss anything *)
exception Unimplemented

datatype regexp =
    Zero
  | One
  | Char of char
  | Plus of regexp * regexp
  | Times of regexp * regexp
  | Star of regexp
  | Whatever
  | Both of regexp * regexp

(* match : regexp -> char list -> (char list -> bool) -> bool *)
(* REQUIRES; p is a total function *)
(* ENSURES: match R L p evaluates to true if there exists L1, L2 such that L=L1@L2 and L1 is in R and p(L2)=true and false otherwise *)
fun match (R : regexp) (L : char list) (p : char list -> bool) : bool =
    case R of
        Zero => false
      | One => p L
      | Char c => (case L of
                     [] => false
                   | c' :: L' => (c = c') andalso p L')
      | Plus (R1, R2) => match R1 L p orelse match R2 L p
      | Times (R1, R2) => match R1 L (fn L' => match R2 L' p)
      | Star R => p L orelse match R L (fn L' => (L <> L') andalso
                                                 match (Star R) L' p)
      (* Task 2.1 *)
      | Whatever => (case L of
                     [] => p L
                   | c'::L' => p L orelse match R L' p)
                   
      (* Task 2.2 *)
      | Both (R1, R2) => match R1 L (fn L' => match R2 L (fn L'' => p L' andalso L' = L''))
                         
fun accept R s = match R (String.explode s) (fn [] => true | _ => false)

(* Task 2.1 Tests *)
val true = accept Whatever "abc"
val true = accept Whatever "Hello World"
val true = accept Whatever ""
val false = match Whatever (String.explode("abc")) (fn x => x = [#"d"])

(* Task 2.2 Tests *)
val R1 = Plus(Char #"a", Plus(Char #"b", Plus(Char #"c", Char #"d")))
val R2 = Plus(Char #"a", Plus(Char #"b", Plus(Char #"x", Char #"y")))
val true = accept (Both(R1,R2)) "a"
val true = accept (Both(R1,R2)) "b"
val false = accept (Both(R1,R2)) "c"
val false = accept (Both(R1,R2)) "y"
val false = accept (Both(R1,R2)) ""

(* Task 3.1 *)
(* halfmatch : regexp -> regexp -> char list -> bool *)
(* REQUIRES: true *)
(* ENSURES: halfmatch R1 R2 L evaluates to true if and only if there exists L1, L2 such that L=L1@L2, length(L1)=length(L2), and L1 is in R1 and L2 is in R2 *)
fun halfmatch (R1 : regexp) (R2 : regexp) (L : char list) : bool = match R1 L (fn L' => match R2 L' (fn L'' => L'' = []) andalso length(L) = 2 * length(L'))

(* Tests for halfmatch *)
val R1 = Star(Plus(Char #"a", Plus(Char #"b", Char #"c")))
val R2 = Star(Plus(Char #"x", Plus(Char #"y", Char #"z")))
val true = halfmatch R1 R2 (String.explode("abcxyz"))
val true = halfmatch R1 R2 (String.explode("abxz"))
val false = halfmatch R1 R2 (String.explode("axy"))
val false = halfmatch R1 R2 (String.explode("abc"))
val false = halfmatch R1 R2 (String.explode("adxy"))
val true = halfmatch R1 R2 (String.explode(""))
