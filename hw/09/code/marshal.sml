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

(********** TASK 2.1 **********)
structure MarshalInt : MARSHAL =
struct

  type t = int
  
  (* write : t -> string *)
  (* REQUIRES: true *)
  (* ENSURES: write(v) evaluates to v marshalled to a string *)
  fun write (v : t) : string = Int.toString(v) ^ ","
  
  (* read : string -> (t * string) option *)
  (* REQUIRES: true *)
  (* ENSURES: read(s) evaluates to SOME (v, s1) if there is a v and s1 such that
   * s = write(v) ^ s1 and NONE otherwise *)
  fun read (s : string) : (t * string) option =
      case Util.peelInt(s) of
          SOME (v, s1) => (case Util.peelOff(",", s1) of
                              SOME (s2) => SOME (v, s2)
                            | _ => NONE)
        | _ => NONE

end

(* Tests for MarshalInt *)
structure TestMarshalInt =
struct
    
  (* Tests for write *)
  val "55," = MarshalInt.write(55)
  val "~55," = MarshalInt.write(~55)
  val "0," = MarshalInt.write(0)
  val "123," = MarshalInt.write(123)
  
  (* Tests for read *)
  val SOME (55,  "") = MarshalInt.read("55,")
  val SOME (55, "hello") = MarshalInt.read("55,hello")
  val NONE = MarshalInt.read("55")
  val NONE = MarshalInt.read("55hello")
  val SOME (~55, "hello") = MarshalInt.read("~55,hello")
  val NONE = MarshalInt.read("-55hello")
  val NONE = MarshalInt.read("hello55")
  val SOME (12, "~100") = MarshalInt.read("12,~100")
  
end


(********** TASK 2.3 **********)
functor MarshalPair (P : MARSHALPAIR) : MARSHAL =
struct

  type t = P.M1.t * P.M2.t
  
  (* write : t -> string *)
  (* REQUIRES: true *)
  (* ENSURES: write(v) evaluates to v marshalled to a string *)
  fun write ((v1, v2) : t) : string = P.M1.write(v1) ^ "," ^ P.M2.write(v2)
  
  (* read : string -> (t * string) option *)
  (* REQUIRES: true *)
  (* ENSURES: read(s) evaluates to SOME (v, s1) if there is a v and s1 such that
   * s = write(v) ^ s1 and NONE otherwise *)
  fun read (s : string) : (t * string) option = 
    case P.M1.read(s) of
        SOME (v, s1) =>
        (case Util.peelOff(",", s1) of
             SOME (s2) =>
             (case P.M2.read(s2) of
                  SOME (u, s3) => SOME ((v,u), s3)
                | _ => NONE)                             
           | _ => NONE)             
                  
      | _ => NONE                        

end

(* Tests for MarshalPair *)
structure TestMarshalPair =
struct
  
  structure MARSHALPAIRINT =
  struct
    structure M1 : MARSHAL = MarshalBool
    structure M2 : MARSHAL = MarshalInt
  end
  
  structure MarshalPairInt = MarshalPair(MARSHALPAIRINT)
  
  (* Tests for write *)
  val "(TRUE),55," = MarshalPairInt.write(true, 55)
  val "(FALSE),55," = MarshalPairInt.write(false, 55)
  val "(TRUE),~55," = MarshalPairInt.write(true, ~55)
  val "(FALSE),0," = MarshalPairInt.write(false ,0)
  val "(TRUE),345," = MarshalPairInt.write(true, 345)
  
  (* Tests for read *)
  val SOME ((true, 55), "") = MarshalPairInt.read("(TRUE),55,")
  val SOME ((false, 55), "") = MarshalPairInt.read("(FALSE),55,")
  val SOME ((true, 55), "hello") = MarshalPairInt.read("(TRUE),55,hello")
  val NONE = MarshalPairInt.read("(TRUE),55")
  val NONE = MarshalPairInt.read("~55hello")
  val NONE = MarshalPairInt.read("hello55")
  val NONE = MarshalPairInt.read("(FALSE)hello")
  val NONE = MarshalPairInt.read("(TRUE)hello55")
  val NONE = MarshalPairInt.read("(TRUE),55")

end


(********** TASK 2.4 **********)
functor MarshalList (S : MARSHAL) : MARSHAL =
struct

  type t = S.t list
  
  (* write : t -> string *)
  (* REQUIRES: true *)
  (* ENSURES: write(v) evaluates to v marshalled to a string *)
  fun write ([] : t) : string = ""
    | write (x::L : t) : string = S.write(x) ^ "," ^ (write L)
  
  (* read : string -> (t * string) option *)
  (* REQUIRES: true *)
  (* ENSURES: read(s) evaluates to SOME (v, s1) if there is a v and s1 such that
   * s = write(v) ^ s1 and NONE otherwise *)
  fun read (s : string) : (t * string) option =
      case S.read(s) of
          SOME (x, s1) =>
          (case Util.peelOff(",", s1) of
               SOME (s2) =>
               (case read(s2) of
                    SOME (L, s3) => SOME (x::L, s3)
                  | _ => SOME ([x], s1))                        
             | _ => SOME ([x], s1))             
        | _ => SOME ([], s)

end

(* Tests for MarshalList *)
structure TestMarshalList =
struct
  
  structure MarshalListInt = MarshalList(MarshalInt)
  
  (* Tests for write *) 
  val "1,,2,,3,," = MarshalListInt.write([1,2,3])
  val "~1,,2,,~3,," = MarshalListInt.write([~1,2,~3])
  val "4,,55,," = MarshalListInt.write([4,55])
  val "" = MarshalListInt.write([])
  
  (* Tests for read *)
  val SOME ([1,2,3], "") = MarshalListInt.read("1,,2,,3,,")
  val SOME ([1,2,3], "hello") = MarshalListInt.read("1,,2,,3,,hello")
  val SOME ([1,2], "3hello") = MarshalListInt.read("1,,2,,3hello")
  val SOME ([~1,2,~3], "hello") = MarshalListInt.read("~1,,2,,~3,,hello")
  val SOME ([4,55], "hello") = MarshalListInt.read("4,,55,,hello")
  val SOME ([], "") = MarshalListInt.read("")
  val SOME ([], "hello") = MarshalListInt.read("hello")
  
end



(********** TASK 2.5 **********)

structure ILL : MARSHAL =
          MarshalList(MarshalList(MarshalInt))

structure ILSB : MARSHAL =
          MarshalPair(struct
            structure M1 = MarshalList(MarshalInt)
            structure M2 = MarshalBool
          end)

structure ISBL : MARSHAL =
          MarshalList(MarshalPair(struct
            structure M1 = MarshalInt
            structure M2 = MarshalBool
          end))

structure ISBLL : MARSHAL = 
          MarshalList(MarshalPair(struct
            structure M1 = MarshalInt
            structure M2 = MarshalList(MarshalBool)
          end))

(********** TASK 2.6 -- extra credit! **********)





(* Here's a bonus example: *)

(*
structure GradesDB =
struct

type grades = (string * (int list * string list)) list

val db : grades =
 [("nkindber", ([95,99,98], (["y","n","y","y"]))),
  ("icw", ([99,99,99], (["y","y","y","y","y"]))),
  ("amyz", ([97,99,99], (["y","y","y","y","y"]))),
  ("yschen", ([100,100,100], (["n","n","n","y","n"]))),
  ("brookes", ([98,98,98], (["y","y","y","y","y"]))),
  ("drl", ([98,100,98], (["y","y","y","y","y"])))
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
*)
