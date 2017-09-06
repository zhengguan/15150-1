
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
      | Whatever => p L orelse (case L of
                                  [] => false
                                | _ :: L' => match Whatever L' p)
      (* Task 2.2 *)
      | Both (R1, R2) =>
        match R1 L (fn L' => match R2 L (fn L'' => L' = L'' andalso p L''))

fun accept R s = match R (String.explode s) (fn [] => true | _ => false)


fun cat rs = foldr (fn (x,y) => Times (x,y)) One rs
fun lit s = cat (map Char (String.explode s))

(* Task 2.1 Tests *)
val rn = Char #"n"
val rm = Char #"m"
val true = accept Whatever ""
val true = accept Whatever "The quick brown"
val true = accept (Times(Whatever, Whatever)) "The quick brown"
val true = accept (Times(Whatever, rn)) "The quick brown"
val true = accept (Plus(Times(Whatever, rm), Times(Whatever, rn))) "The quick brown"
val true = accept (Times(Char#"T", Whatever)) "The quick brown"
val false = accept (Times(Char#"t", Whatever)) "The quick brown"
val false = accept (Times(rn, Whatever)) ""


(* Task 2.2 Tests *)
val ra = Char #"a"
val rb = Char #"b"
val true = accept (Both(Plus(ra, rb), ra)) "a"
val true = accept (Both(Star ra, Whatever)) "aaaaaa"
val false = accept (Both(Times(Whatever, ra), Times(Whatever, rb))) "cccccab"
val false = accept (Times(Both(ra,Times(ra,rb)),Whatever)) "abcccc"
(* Tricky Case *)
val tricky = Times(Both(Times(Whatever, rb), (Star ra)), Whatever)
val false = accept tricky "aaabaa"

(* Task 3.1 *)
(* halfmatch : regexp -> regexp -> char list -> bool
 * REQUIRES : true
 * ENSURES : (halfmatch R1 R2 L ) evalutes to true iff there exist an
 *     L1 and L2 such that
              L = L1 @ L2
              length(L1) = length (L2)
              L1 is in the language of R1 and L2 is in the language of R2
 *)
fun halfmatch (R1 : regexp) (R2 : regexp) (L : char list) : bool =
    let
      val n = length L
    in
      (n mod 2 = 0) andalso
      match R1 L
            (fn L' => match R2 L'
                            (fn L'' => null L'' andalso length L' = n div 2))
    end

val anbn = halfmatch (Star(Char #"0")) (Star(Char #"1"))
val true = anbn (String.explode "")
val true = anbn (String.explode "01")
val true = anbn (String.explode "00001111")
val false = anbn (String.explode "0")
val false = anbn (String.explode "1")
val false = anbn (String.explode "1100")
val false = anbn (String.explode "001")
val false = anbn (String.explode "011")
val false = anbn (String.explode "11")
