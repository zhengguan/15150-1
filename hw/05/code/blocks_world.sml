(* ---------------------------------------------------------------------- *)
(* Optional Block World Tasks

   In the following code there are some TODO's. For each of these TODO's, you
   need to replace the exceptions and english text with your datatype
   constructor.
*)

fun stepMany (moves : move list, s : state) : state option =
    case moves of
        [] => SOME s
      | m :: ms => (case step (m,s) of NONE => NONE | SOME s' => stepMany (ms , s'))

local

fun parseBlock (s : string) : block option =
    case s of
        "A" => SOME (raise Fail "block A") (* TODO *)
      | "B" => SOME (raise Fail "block B") (* TODO *)
      | "C" => SOME (raise Fail "block C") (* TODO *)
      | _ => NONE

fun parseMove (s : string) : move option =
    case String.tokens (fn x => x = #" ") s of
        [ "pickup" , x , "from" , "table" ] =>
            (case parseBlock x of
                 SOME b => SOME (raise Fail "pick up b from table") (* TODO *)
               | NONE => NONE)
      | [ "put" , x , "on" , "table" ] =>
            (case parseBlock x of
                 SOME b => SOME (raise Fail "put b on table") (* TODO *)
               | NONE => NONE)
      | [ "pickup" , x , "from" , y ] =>
             (case (parseBlock x,parseBlock y) of
                 (SOME a,SOME b) => SOME (raise Fail "pick up a from b") (* TODO *)
               | _ => NONE)
      | [ "put" , x , "on" , y ] =>
            (case (parseBlock x,parseBlock y) of
                 (SOME a,SOME b) => SOME(raise Fail "put a on b") (* TODO *)
               | _ => NONE)
      | _ => NONE

fun transpose (m : 'a list list) : 'a list list =
    case m of
        [] => []
      | [] :: rs => []
      | _ => (map hd m) :: transpose (map tl m)


local
    fun enumerateBlocksOnTable (s : state) : block list =
        List.map (fn f => case f of
                    (* TODO add a case for 'b is on the table' => b *)
                    _ => raise Fail "impossible")
        (List.filter (fn f => case f of
                      (* TODO add a case for 'b is on the table' => true *)
                       _ => false) s)

    type tower = block list
    fun tower (s : state, sofar : block list, cur : block) : tower =
        case extract ((fn f => case f of
                               (* TODO add a case for 'a is on b' => b = cur *)
                              _ => false), s) of
            NONE => sofar
          (* TODO add a case for SOME ('a is on b',s') => tower (s' , a :: sofar , a) *)
          | SOME _ => raise Fail "impossible"

    fun getBlockInHand (s : state) : block option =
        case List.find (fn x => case x of
                                   (* TODO add a case for 'the hand holds b' => true *)
                                   _ => false) s of
            (*TODO add a case for SOME ('the hand holds b') => SOME b *)
            SOME _ => raise Fail "impossible"
          | NONE => NONE

    fun showBlock (b : block) : string list =
        let fun square s = ["---",
                           "|"^s^"|",
                            "---"]
        in
            case b of
            (* TODO
               'block A' => square "A"
              | 'block B' => square "B"
              | 'block C' => square "C" *)
               _ => raise Fail "this will be redundant when you're done"
        end

    (* each elt of the resulting list has length 3 *)
    fun showTower (t : tower) : string list = List.foldr (fn (b,y) => showBlock b @ y) [] t

    (* output is rectangular *)
    fun showTowers (ts : tower list, inhand : block option) : string list list =
        let val tstrings = List.map showTower ts
            val maxlen = List.foldr Int.max 0 (List.map List.length tstrings)

            val (tstrings',maxlen') = case inhand of
                                       NONE => (tstrings,maxlen)
                                     | SOME b => ((("/|\\" :: showBlock b) @ (List.tabulate (maxlen, fn _ => "   ")))
                                                  :: tstrings, maxlen + 4)
            fun pad t = (List.tabulate (maxlen' - List.length t, fn _ => "   ")) @ t
        in
            List.foldr (fn (t,tstrs) => pad t :: List.tabulate (maxlen', fn _ => "   ") :: tstrs) [] tstrings'
        end

in
    fun showState (s : state) : string =
        let val towers = (List.map (fn b => tower (s,[b],b)) (enumerateBlocksOnTable s))
            val lines = (List.map String.concat (transpose (showTowers (towers, getBlockInHand s))))
        in
            "\n" ^ (List.foldr (fn (s,s') => s ^ "\n" ^ s')
                               (String.concat (List.tabulate (25, fn _ => "-")))
                               lines) ^ "\n"
        end
end

fun loop (s : state) : unit =
    let val () = print (showState s)
        val () = print "Next move: "
    in
        case (TextIO.inputLine TextIO.stdIn) of
            NONE => (print "no input\n"; loop s)
          | SOME "quit\n" => ()
          | SOME input =>
                (case parseMove (String.substring (input,0,String.size input - 1)) of
                     NONE => (print "Move did not parse\n"; loop s)
                   | SOME m => (case step (m,s) of
                                    NONE => (print "Cannot make that move\n"; loop s)
                                  | SOME s' => loop s'))
    end

val help = "\nPossible moves:\n" ^
           "  pickup <block> from table\n" ^
           "  put <block> on table\n" ^
           "  pickup <block> from <block>\n" ^
           "  put <block> on <block>\n" ^
           "  quit\n"
in
    fun playBlocks() = let val () = print help in loop initial end
end
