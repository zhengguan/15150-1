(* We have provided two maps to test the game on. You may construct
 * your own if you wish to battle on more interesting graphs! *)
functor M1 (P : Printer) : MAP =
struct
    val inp = [
               ("Land0", [1, 5]),
               ("Land1", [2, 0]),
               ("Land2", [3, 1]),
               ("Land3", [4, 2]),
               ("Land4", [5, 3]),
               ("Land5", [0, 4])
              ]

    fun listToSeq ([] : 'a list) : 'a Seq.seq = Seq.empty ()
      | listToSeq (x::L) = Seq.cons x (listToSeq L)

    val board = listToSeq (List.map (fn (st, ll) => (st, listToSeq ll)) inp)

    fun board_to_string ss =
      let val ss = P.padAll (P.labelAll ss)
      in Seq.reduce (fn (a, b) => a ^ "<-->" ^ b) "" ss
      end

    val min_start = 0
    val max_start = 3
    val max_terri = 9

    val max_units = 40

end

structure M2 : MAP =
struct
    (* 0  1  2  3
     * 4  5  6  7
     * 8  9  10 11
     * 12 13 14 15 *)
    val inp = [
               ("", [1, 4]),
               ("", [0, 2, 5]),
               ("", [1, 3, 6]),
               ("", [2, 7]),
               ("", [0, 5, 8]),
               ("", [1, 4, 6, 9]),
               ("", [2, 5, 7, 10]),
               ("", [3, 6, 11]),
               ("", [4, 9, 12]),
               ("", [5, 8, 10, 13]),
               ("", [6, 9, 11, 14]),
               ("", [7, 10, 15]),
               ("", [8, 13]),
               ("", [9, 12, 14]),
               ("", [10, 13, 15]),
               ("", [11, 14])
              ]

    fun listToSeq [] = Seq.empty ()
      | listToSeq (x::L) = Seq.cons x (listToSeq L)

    val board = listToSeq (List.map (fn (st, ll) => (st, listToSeq ll)) inp)

    fun board_to_string ss =
      let val ss = P.padAll (P.labelAll ss)
      in Seq.reduce op^ ""
        (Seq.tabulate (fn i => (Seq.nth i ss) ^ (if (i + 1) mod 4 = 0
                                                 then "\n"
                                                 else " "))
                                            (Seq.length ss))
      end

    val min_start = 0
    val max_start = 15
    val max_terri = 10

    val max_units = 80

end

functor Riskless(Dim : MAP) : GAME
    where type move = int * int =
struct
    open Seq
    exception IllegalState
    type vertex = int
    datatype player = Minnie | Maxie
    datatype outcome = Winner of player | Draw
    datatype status = Over of outcome | In_play

    datatype position = Min of int | Max of int | Empty

    structure Est = Estimate

    (* The string is simply there to give every territory a name *)
    datatype territory = T of string * position

    datatype gstate = B of (territory * vertex seq) seq * player
    type state = gstate

    type move = int * int

    val start =
        let
          fun nodeVal i =
            case (i = Dim.min_start, i = Dim.max_start) of
                 (false, false) => Empty
               | (false, true) => Max (1)
               | (true, false) => Min (2)
               | (true, true) => raise IllegalState
        in
          B (tabulate (fn i => let val (st, ss) = nth i Dim.board
                            in (T (st, nodeVal i), ss)
                            end) (length Dim.board), Maxie)
        end


    (* Task 4.1 - Implement these functions below! *)
    (* status : state -> status *)
    (* REQUIRES: true *)
    (* ENSURES: Given a game state s, status s evaluates to the current
     * status of the game *)
    fun status (B(b, p)) = 
        let
          val (max1, max2) = mapreduce (fn (T(s, p), S) =>
                                 case p of
                                     Max(x) => (true, x)
                                   | Min(_) => (false, 0)
                                   | Empty => (true, 0))
                                 (true, 0)
                                 (fn ((x,y), (w,z)) => ((x andalso w),
                                                        (y + z)))
                                 b
          val max = max1 orelse (max2 >= Dim.max_units)
          val (min1, min2) = mapreduce (fn (T(s, p), S) =>
                                 case p of
                                     Min(x) => (true, x)
                                   | Max(_) => (false, 0)
                                   | Empty => (true, 0))
                                 (true, 0)
                                 (fn ((x,y), (w,z)) => ((x andalso w),
                                                        (y + z)))
                                 b
          val min = min1 orelse (min2 >= Dim.max_units)
        in
          case (min, max) of
              (true, true) => Over(Draw)
            | (true, false) => Over(Winner(Minnie))
            | (false, true) => Over(Winner(Maxie))
            | _ => In_play
        end

    (* moves : state -> move Seq.seq *)
    (* REQUIRES: true *)
    (* ENSURES: moves s  evaluates to a sequence of valid moves for s *)
    fun moves (B(b, p)) =
        case status(B(b, p)) of
            In_play =>
              let 
                val m = case p of 
                    Minnie =>
                    tabulate (fn i =>
                              case (nth i b) of
                                  (T(_, Min(0)), _) => empty()
                                | (T(_, Min(_)), v) => map (fn j => (i,j)) v
                                | _ => empty())
                             (length b)
                  | Maxie =>
                    tabulate (fn i =>
                              case (nth i b) of
                                  (T(_, Max(0)), v) => empty()
                                | (T(_, Max(_)), v) => map (fn j => (i,j)) v
                                | _ => empty())
                             (length b)
                in
                  flatten m
                end
          | _ => empty()   
     
    (* player : state -> player *)
    (* REQUIRES: true *)
    (* ENSURES: Returns the player making a move in the current state *) 
    fun player (B(_, p)) = p

    (* make_move : (state * move) -> state *)
    (* REQUIRES: Must be a valid move for the current state *)
    (* ENSURES: Evaluates to the sta11te after the move has bene applied *)
    fun make_move (B(b, p), (x,y)) =
        let
          val not_p = case p of
                          Minnie => Maxie
                        | Maxie => Minnie
          val (T(x_str,  x_pos), x_neigh) = nth x b
          val x_size = case x_pos of
                           Empty => 0
                         | Min(x) => x
                         | Max(x) => x
          val (T(y_str,  y_pos), y_neigh) = nth y b
          val (y_p, y_size) = case y_pos of
                                  Empty => (p, 0)
                                | Min(x) => (Minnie, x)
                                | Max(x) => (Maxie, x)
          val (new_y_p, new_y_size) =
              case ((p = y_p), Int.compare(x_size, y_size)) of
                  (true, _) => (p, x_size + y_size)
                | (_, LESS) => (not_p, y_size - x_size)
                | _ => (p, x_size - y_size)
          val new_b = tabulate (fn i =>
                                case ((i = x), (i = y)) of
                                    (true, _) =>
                                    (T(x_str,
                                       case p of
                                           Minnie => Min(0)
                                         | Maxie => Max(0)),
                                     x_neigh)
                                  | (_, true) =>
                                    (T(y_str,
                                       case new_y_p of
                                           Minnie => Min(new_y_size)
                                         | Maxie => Max(new_y_size)),
                                     y_neigh)
                                  | _ => nth i b)
                                (length b) 
        in
          B((map (fn b =>
                  case b of
                      (T(str, Empty), neigh) => (T(str, Empty), neigh)
                    | (T(str, Min(x)), neigh) =>
                      (T(str, Min(Int.min(x+1, Dim.max_terri))), neigh)
                    | (T(str, Max(x)), neigh) =>
                      (T(str, Max(Int.min(x+1, Dim.max_terri))), neigh))
                 new_b),
            not_p)
        end 
                                
    type est = Est.est

    (* Please describe your implementation of estimate below:
     *
     *)
    (* estimate : state -> Est.est *)
    (* REQUIRES: true *)
    (* ENSURES: Returns an estimated score for a given state *)
    fun estimate (B(b, _)) =
        reduce (fn (x,y) => x + y) 0
               (tabulate (fn i => 
                          case (nth i b) of
                              (T(_,  Empty), _) => 0
                            | (T(_,  Min(x)), _) => ~x - 1
                            | (T(_,  Max(x)), _) => x + 1)
                         (length b))   
                        
    (* Functions to help visualize the game *)
    fun move_to_string (i, j) =
        "(" ^ (Int.toString i) ^ ", " ^ (Int.toString j) ^ ")"

    fun territory_to_string (i, (T (n, p))) =
        (Int.toString i) ^ ": " ^ n ^ "-- " ^
        (case p of
           Min (v) => "Minnie (" ^ (Int.toString v) ^ ")"
         | Max (v) => "Maxie  (" ^ (Int.toString v) ^ ")"
         | _ => "Unclaimed")

    fun neighbors_to_string (B (ts, _)) ss =
        mapreduce (fn i => let val (T (st, _), _) = nth i ts
                           in "\t" ^ (Int.toString i) ^ ": " ^ st
                           end) ""
                           (fn (a, b) => a ^ "\n" ^ b) ss

    fun valid_move ((s, t), G) =
        case nth s G of
             (T (_, Empty), _) => false
           | (_, S) => mapreduce (fn x => x = t) false
                                 (fn (a, b) => a orelse b) S

    fun parse_move (B (t, _)) s =
        case String.tokens (fn #" " => true | _ => false) s of
            [si, sj] =>
              (case (Int.fromString si, Int.fromString sj) of
                   (SOME i, SOME j) =>
                       if valid_move ((i, j), t)
                       then SOME (i, j)
                       else NONE
                 | (_, _) => NONE)
          | _ => NONE

    fun pos_to_string (T (s, Empty), _) = s ^ ":"
      | pos_to_string (T (s, Min(i)), _) = s ^ ": O(" ^ (Int.toString i) ^ ")"
      | pos_to_string (T
 (s, Max(i)), _) = s ^ ": X(" ^ (Int.toString i) ^ ")"

    fun state_to_string (board as (B (ts, _))) =
        Dim.board_to_string (map pos_to_string ts)

end

(* Build Riskless game for each map *)
structure RL1 = Riskless (M1 (P))
structure RL2 = Riskless (M2)

structure TestRiskless =
struct
  
  (* Generates a sequence from a list *)
  fun seqFromList (l : 'a list) : 'a Seq.seq =
      Seq.tabulate (fn i => List.nth (l, i)) (List.length l)
      
  (* Checks if two sequences are equal, given equality function p *)
  fun seqEq (p: 'a * 'a -> bool) (s1 : 'a Seq.seq, s2 : 'a Seq.seq) : bool =
      if Seq.length s1 <> Seq.length s2 then false
      else Seq.mapreduce p true (fn (x,y) => x andalso y) (Seq.zip (s1, s2))
      
  (* Tests for status *)
  val In_Play = RL1.status(RL1.start)
  val In_Play = RL2.status(RL2.start)
  
  (* Tests for moves *)
  val true = seqEq (fn (x,y) => x = y)
                   (seqFromList([(3,4), (3,2)]), (RL1.moves (RL1.start)))
  val true = seqEq (fn (x,y) => x = y)
                   (seqFromList([(15,11), (15,14)]), (RL2.moves (RL2.start)))
                   
  (* Tests for player *)
  val Maxie = RL1.player(RL1.start)
  val Maxie = RL2.player(RL2.start)
  
  (* Test for make_move *)
  val moveRL1 = RL1.make_move((RL1.start), (3,4))
  val moveRL2 = RL2.make_move((RL2.start), (15,11))
  val In_Play = RL1.status(moveRL1)
  val true = seqEq (fn (x,y) => x = y)
                   (seqFromList([(0,1), (0,5)]), (RL1.moves (moveRL1)))
  val Minnie = RL1.player(moveRL1)
  val In_Play = RL2.status(moveRL2)
  val true = seqEq (fn (x,y) => x = y)
                   (seqFromList([(0,1), (0,4)]), (RL2.moves (moveRL2)))
  val Minnie = RL2.player(moveRL2)
  
  (* Tests for player *)
  val ~1 = RL1.estimate(RL1.start)
  val ~1 = RL2.estimate(RL2.start)
  
end
