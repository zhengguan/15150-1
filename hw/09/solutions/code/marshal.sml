structure MarshalBool : MARSHAL =
struct
    type t = bool

    fun write b =
        case b
         of true => "(TRUE)"
          | false => "(FALSE)"

    fun read s =
        case Util.peelOff("(TRUE)",s)
         of SOME s => SOME (true,s)
          | NONE =>
            (case Util.peelOff("(FALSE)",s)
              of SOME s => SOME (false,s)
               | NONE => NONE)
end


(********** TASK 5.1 **********)
structure MarshalInt : MARSHAL =
struct
    type t = int

    (* purpose:
     * REQUIRES: True
     * ENSURES: write i evaluates to a string representation of i such that
     * read (write i ^ s) = SOME(i, s)
     *)
    fun write i =  Int.toString i ^ "."

    (* REQUIRES: s is some string of the form write i ^ t for i : int and
     * t : string
     * ENSURES:
     * read s evaluates to the digit represented by s and the rest of s,
     * possibly empty.
     *)
    fun read s =
        case Util.peelInt s
         of SOME (i , s) =>
            (case Util.peelOff (".",s)
              of SOME x => SOME (i, x)
               | NONE => NONE)
          | NONE => NONE

end

(********** TASK 5.2 **********)
functor MarshalPair (P : MARSHALPAIR) : MARSHAL =
struct
    type t = P.M1.t * P.M2.t

    (* purpose:
     * REQUIRES: True
     * ENSURES: write (x,y) evaluates to a string representation of (x,y)
     * such that read (write (x, y) ^ s) = SOME ((x, y), s)
     *)
    fun write (x,y) = (P.M1.write x) ^ P.M2.write y

    (* purpose:
     * REQUIRES: s is some string of the form write (x, y) ^ t for all
     * (x, y) : P.S1.t * P.S2.t and t : string
     * ENSURES: read s parses a pair from the string representation
     * written above
     *)
    fun read (s : string) : (t * string) option  =
        (case P.M1.read s
             of SOME (x , s) =>
                 (case P.M2.read s of
                      SOME (y , s) => SOME((x,y),s)
                    | NONE => NONE)
           | NONE => NONE)
end

(********** TASK 5.3 **********)
functor MarshalList (S : MARSHAL) : MARSHAL =
struct
    type t = S.t list

    (* REQUIRES: True
     * ENSURES: write l evaluates to a string representation of l such that
     * read (write l ^ s) = SOME(l, s)
     *)
    fun write l =
        case l
         of [] => "(NIL)"
          | x :: xs => "(CONS " ^ (S.write x) ^ " " ^ write xs ^ ")"

    (* REQUIRES: S is of the form write l ^ t for some t : string
     * ENSURES:
     * read s evaluates to the list represented by s and the rest of s,
     * possibly empty.
     *)
    fun read (s : string) : (S.t list * string) option  =
        case Util.peelOff("(NIL)",s) of
            SOME s => SOME ([] , s)
          | NONE =>
                (case (Util.peelOff ("(CONS " , s))
                  of SOME s =>
                     (case S.read s
                       of SOME (x , s) =>
                          (case Util.peelOff(" ",s)
                            of SOME s =>
                               (case read s
                                 of SOME (xs , s) =>
                                    (case Util.peelOff (")" , s)
                                      of SOME s => (SOME (x :: xs, s))
                                       | NONE => NONE)
                                  | NONE => NONE)
                             | NONE => NONE)
                        | NONE => NONE)
                   | NONE => NONE)

    (* here's a monadic version of the same code *)

    val RETURN = SOME
    infix 8 THEN
    fun (x : 'a option) THEN (f : 'a -> 'b option)
      : 'b option =
        case x of
            NONE => NONE
          | SOME v => f v

    fun read (s : string) : (S.t list * string) option  =
        case Util.peelOff("(NIL)",s) of
            SOME s => SOME ([] , s)
          | NONE =>
                (Util.peelOff ("(CONS " , s)) THEN (fn s =>
                S.read s                      THEN (fn (x , s) =>
                Util.peelOff(" ",s)           THEN (fn s =>
                read s                        THEN (fn (xs , s) =>
                Util.peelOff (")" , s)        THEN (fn s =>
                RETURN (x :: xs, s))))))
end

(********** TASK 5.3 **********)
functor MarshalListLen (S : MARSHAL) : MARSHAL =
struct
    type t = S.t list

    fun ocase f x =
        case x
         of SOME v => f v
          | NONE => NONE

    fun ocons (x,y) = ocase (fn (l,v) => SOME(x::l,v)) y

    fun write l =
        (MarshalInt.write (length l)) ^
        (foldr (fn (a,b) => (S.write a) ^ b) "" l)

    (* same idea but with the common case-statements factored out. *)
    fun read s =
        let
          fun recons (n : int, s : string) =
              case n
               of 0 => SOME([], s)
                | _ => ocase
                           (fn (elem,rest) => ocons(elem, recons (n-1, rest)))
                           (S.read s)
        in
          ocase recons (MarshalInt.read s)
        end
end

(********** TASK 5.4 **********)

structure ILL : MARSHAL = MarshalList(MarshalList(MarshalInt))
structure ILSB : MARSHAL = MarshalPair(struct
                                       structure M1=MarshalList(MarshalInt)
                                       structure M2=MarshalBool
                                       end)
structure ISBL : MARSHAL = MarshalList(
                            MarshalPair(struct
                                        structure M1=MarshalInt
                                        structure M2=MarshalBool
                                        end))
structure ISBLL : MARSHAL = MarshalList(
                             MarshalPair(struct
                                         structure M1=MarshalInt
                                         structure M2=MarshalList(MarshalBool)
                                         end))

(********** TASK 5.5 -- extra credit! **********)
structure MarshalString : MARSHAL =
struct
    structure S = MarshalList (MarshalInt)
    type t = string

    fun write s = S.write(map ord (explode s))
    fun read l = Option.map (fn (s,y) => (implode (map chr s), y)) (S.read l)
end

(* Here's a bonus example: *)

structure GradesDB =
struct

type grades = (string * (int list * string list)) list

val db : grades =
 [("drl", ([95,99,98], (["y","n","y","y"]))),
  ("iev", ([99,99,99], (["y","y","y","y","y"]))),
  ("nkindber", ([97,99,99], (["y","y","y","y","y"]))),
  ("srikrish", ([100,100,100], (["n","n","n","y","n"]))),
  ("rmemon", ([98,98,98], (["y","y","y","y","y"]))),
  ("rmurcek", ([98,100,98], (["y","y","y","y","y"])))
  ]
structure SG =
MarshalList(
  MarshalPair(
  struct
    structure M1 = MarshalString
    structure M2 = MarshalPair(
                   struct
                    structure M1 = MarshalList(MarshalInt)
                    structure M2 = MarshalList(MarshalString)
                   end)
  end))


val sdb = SG.write db
val db' = SG.read sdb

end


(* Some code we used to test *)

signature UNITPACK =
sig
  val s : string
end

functor MarshalUnit(P : UNITPACK) : MARSHAL =
struct
  type t = unit
  fun write _ = P.s
  fun read s =
      case Util.peelOff(P.s,s)
       of SOME s => SOME ((),s)
        | NONE => NONE
end

structure SUBracks = MarshalUnit (struct val s = "[]" end)
structure SUEndBrack = MarshalUnit (struct val s = "]" end)
structure SUOpenBrack = MarshalUnit (struct val s = "[" end)
structure SUPnilP = MarshalUnit (struct val s = "(NIL)" end)
structure SUNil = MarshalUnit (struct val s = "NIL" end)
structure SU40 = MarshalUnit (struct val s = "40" end)
structure SUP40P = MarshalUnit (struct val s = "(40)" end)
structure SUComma = MarshalUnit (struct val s = "," end)
structure SUEpsilon = MarshalUnit (struct val s = "" end)

(* structure S = MarshalList(MarshalList(SUEpsilon)) *)

