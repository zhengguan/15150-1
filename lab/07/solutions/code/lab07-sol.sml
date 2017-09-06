(*********************** Task 3 ***********************)

(* Datatype definition for polymorphic trees *)
datatype 'a tree = Empty
   | Node of 'a tree * 'a * 'a tree

(* Task 3.1 *)
(*
 * size : 'a tree -> (int -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES: size T k => k applied to the number of elements in T
 *)
fun size (Empty : 'a tree) (k : int -> 'a) : 'a = k 0
  | size (Node(l,_,r)) k = size l (fn x => size r (fn y => k (x + y + 1)))

(* Test cases *)
val 0 = size (Empty) (fn x => x)
val 1 = size (Node(Empty, 1, Empty)) (fn x => x)
val 2 = size (Node(Node(Empty, 1, Empty), 1, Empty)) (fn x => x)

(* -------------------- Task 4 -------------------- *)

(* Datatype definition for regular expressions as defined in lecture *)
datatype regexp = Zero
   | One
   | Char of char
   | Plus of regexp * regexp
   | Times of regexp * regexp
   | Star of regexp

(* Task 4.1 *)
(*
 * anyChar : char list -> regexp
 * REQUIRES: true
 * ENSURES: anyChar L => R s.t. accepts R [#"c"] = true for any #"c" in L
*)
fun anyChar (L : char list) : regexp =
    foldr (fn (c,R) => Plus(Char c, R)) (Zero) L

(* Test cases *)
val Zero = anyChar []
val Plus(Char #"c", Zero) = anyChar [#"c"]
val Plus(Char #"p", Plus(Char #"l", Zero)) = anyChar [#"p",#"l"]

(* Task 4.2 *)
(*
 * fromString : string -> regexp
 * REQUIRES: true
 * ENSURES: fromString s => R such that accepts R (String.explode s) = true
 *)
fun fromString (s : string) : regexp =
    foldr (fn (c,R) => Times(Char c, R)) One (String.explode s)

(* Test cases *)
val One = fromString ""
val Times(Char #"c", One) = fromString "c"
val Times(Char #"c", Times(Char #"s", One)) = fromString "cs"

(* Task 4.3 *)
(*
 * anyString : string list -> regexp
 * REQUIRES: true
 * ENSURES: anyString L => R such that accepts R S = true iff S in L
*)
fun anyString (L : string list) : regexp =
    foldr (fn (s,R) => Plus(fromString s, R)) Zero L

(* Test cases *)
val Zero = anyString []
val Plus(Times(Char #"c", Times(Char #"s", One)), Zero) = anyString ["cs"]
val x = anyString ["cs"]

val validChars =
    String.explode "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-"

(* Task 4.4 *)
(*
 * emailer : string -> regexp
 * REQUIRES: true
 * ENSURES: emailer d => R such that accepts R S = true iff
            S is an email address of the form s ^ "@" ^ d for some s
 *)
fun emailer (d : string) : regexp =
    Times(Times(Star(anyChar validChars), Char(#"@")),
          Times(Star(anyChar validChars), fromString d))


(********************** Task 5 ********************)

(* Type definition for polynomials as defined in handout *)
type poly = int -> real

(* Task 5.1 *)
(*
 * add : poly * poly -> poly
 * REQUIRES: true
 * ENSURES: add (p1, p2) => polynomial sum of p1 and p2
 *)
fun add (p1 : poly, p2 : poly) : poly = (fn x => p1 x + p2 x)

(* Test cases *)
val p1 = (fn x => real x)
val p2 = (fn x => real x)
val p3 = add (p1, p2)
val true = Real.==(2.0, p3 1)

(* Task 5.2 *)
(*
 * mult : poly * poly -> poly
 * REQUIRES: true
 * ENSURES: mult (p1, p2) => polynomial product of p1 and p2
 *)
fun mult (p1 : poly, p2 : poly) : poly =
    (fn x => foldr op+ 0.0
                   (List.tabulate(x+1, (fn y => (p1 y) * (p2 (x-y))))))

(* Test cases *)
val p1 = (fn x => real x)
val p2 = (fn x => (real x) + 1.0)
val p3 = mult(p1, p2)
val true = Real.==(0.0, p3 0)
val true = Real.==(1.0, p3 1)
val true = Real.==(4.0, p3 2)

(* Task 5.3 *)
(*
 * eval : poly -> int -> real -> real
 * REQUIRES: true
 * ENSURES: eval f n x => sum of the first n terms of f evaluated at x
 *)
fun eval (p : poly) (0 : int) (x : real) : real = p 0
  | eval p n x = (p n) * Math.pow(x, (real n)) + eval p (n-1) x

(* Test cases *)
val true = Real.==(3.0, eval (fn x => real(x)) 2 1.0)
val true = Real.==(1.0, eval (fn x => real(x)+1.0) 0 2.5)
