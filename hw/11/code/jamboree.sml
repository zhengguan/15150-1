functor Jamboree (Settings : sig
                                 structure G : GAME
                                 val search_depth : int
                                 val prune_percentage : real
                             end) : PLAYER  =
struct
  structure Game = Settings.G

  type edge = Game.move * Game.est
  datatype value = Step of edge
                 | Leaf of Game.est

  fun valueToString v =
      case v of
        Leaf e => "Leaf(" ^ (Game.Est.toString e) ^ ")"
      | Step (m, e) => "Step(" ^ (Game.move_to_string m) ^ ", " ^
                       (Game.Est.toString e) ^ ")"

  type alphabeta = value * value (* invariant : alpha < beta *)
  fun abToString (a,b) = "(" ^ valueToString a ^ "," ^ valueToString b ^ ")"

  fun valueOf (Step (_, e)) = e
    | valueOf (Leaf e) = e

  fun moveOf (Step (m, _)) = m
    | moveOf _ = raise Fail "no moves!"

  fun valuecmp (x, y) = Game.Est.compare (valueOf x, valueOf y)

  fun maxalpha (x, y) =
      case (valuecmp (x,y), (x, y)) of
        (GREATER, _) => x
      | (LESS, _) => y
      | (EQUAL, (Step _, Leaf _)) => x
      | (EQUAL, (Leaf _, Step _)) => y
      | _ => x

  fun minbeta (x, y) =
      case (valuecmp (x,y), (x, y)) of
        (GREATER, _) => y
      | (LESS, _) => x
      | (EQUAL, (Step _, Leaf _)) => x
      | (EQUAL, (Leaf _, Step _)) => y
      | _ => x


  (* Task 6.1 *)
  val initialAB : alphabeta = (Leaf(Game.Est.minnie_wins), Leaf(Game.Est.maxie_wins))

  (* Task 6.2 *)
  fun splitMoves (s : Game.state) =
      let
        val move_seq = Game.moves(s)
        val n = Seq.length(move_seq)
        val ab_num = (Real.floor (Settings.prune_percentage * Real.fromInt(n)))
        val mm_num = n - ab_num
      in
        ((Seq.tabulate (fn i => Seq.nth i move_seq) ab_num),
         (Seq.tabulate (fn i => Seq.nth (i + ab_num) move_seq) mm_num))
      end

  (* Task 6.3 *)
  (* F : int -> Game.state -> alphabeta -> value *)
  (* REQUIRES: d > 0, status s is In_Play, and Game.moves s evaluates to a
   * non-empty sequence of valid moves *)
  (* ENSURES: Computes the best possible game result given a game state s
   * at a depth d for Maxie *)
  fun F (d : int) (s : Game.state) ((a, b) : alphabeta) : value =
      case Game.status(s) of
          Game.In_play => (case d of
                               0 => Leaf(Game.estimate(s))
                             | _ => let
                                      val (abmoves, mmmoves) = splitMoves(s)
                                      val m = if (Seq.length(abmoves) <> 0)
                                                then Seq.nth 0 abmoves 
                                                else Seq.nth 0 mmmoves
                                    in
                                      updateA d (Seq.showl(abmoves)) mmmoves
                                              s m (a, b)
                                    end)
        | Game.Over(Game.Winner(Game.Minnie)) =>
          Leaf(Game.Est.minnie_wins)
        | Game.Over(Game.Winner(Game.Maxie)) =>
          Leaf(Game.Est.maxie_wins)
        | Game.Over(Game.Draw) => Leaf(Game.Est.draw)

  (* G : int -> Game.state -> alphabeta -> value *)
  (* REQUIRES: d > 0, status s is In_Play, and Game.moves s evaluates to a
   * non-empty sequence of valid moves *)
  (* ENSURES: Computes the best possible game result given a game state s
   * at a depth d for Minnie *)
  and G (d : int) (s : Game.state) ((a, b)  : alphabeta) : value =
      case Game.status(s) of
          Game.In_play => (case d of
                               0 => Leaf(Game.estimate(s))
                             | _ => let
                                      val (abmoves, mmmoves) = splitMoves(s)
                                      val m = if (Seq.length(abmoves) <> 0)
                                                then Seq.nth 0 abmoves 
                                                else Seq.nth 0 mmmoves
                                    in
                                      updateB d (Seq.showl(abmoves)) mmmoves
                                              s m (a, b)
                                    end)
        | Game.Over(Game.Winner(Game.Minnie)) =>
          Leaf(Game.Est.minnie_wins)
        | Game.Over(Game.Winner(Game.Maxie)) =>
          Leaf(Game.Est.maxie_wins)
        | Game.Over(Game.Draw) => Leaf(Game.Est.draw)
               
  (* updateA : int -> Game.move Seq.lview -> Game.move Seq.seq ->
   * Game.state -> Game.move -> alphabeta *) 
  (* REQUIRES: d > 0, status s is In_Play, Game.moves s evaluates to a
   * non-empty sequence of valid moves that includes m *)
  (* ENSURES: Computes the best possible game result given a game state s
   * at a depth d for Maxie *)                         
  and updateA (d : int) (abmoves : Game.move Seq.lview)
              (mmmoves : Game.move Seq.seq) (s : Game.state)
              (m : Game.move) ((a, b) : alphabeta) : value =
              case abmoves of
                  Seq.Nil => Seq.mapreduce
                             (fn x => G (d-1) (Game.make_move(s, x)) (a, b))
                             (Step(m, valueOf(b)))
                             (fn (x,y) => case valuecmp(x,y) of
                                              LESS => y
                                            | _ => x)
                             mmmoves
                | Seq.Cons(x, xs) =>
                  let
                    val new_a = G (d-1) (Game.make_move(s, x)) (a, b)
                  in
                    case (valuecmp(new_a, b),
                          valuecmp(new_a, a)) of
                        (LESS, LESS) => 
                        updateA d (Seq.showl(xs)) mmmoves s m (a, b)
                      | (LESS, _) => 
                        updateA d (Seq.showl(xs)) mmmoves s x (new_a, b)
                      | _ => Step(x, Game.Est.maxie_wins)
                  end

  (* updateB : int -> Game.move Seq.lview -> Game.move Seq.seq ->
   * Game.state -> Game.move -> alphabeta *) 
  (* REQUIRES: d > 0, status s is In_Play, Game.moves s evaluates to a
   * non-empty sequence of valid moves that includes m *)
  (* ENSURES: Computes the best possible game result given a game state s
   * at a depth d for Minnie *)                       
  and updateB (d : int) (abmoves : Game.move Seq.lview)
              (mmmoves : Game.move Seq.seq) (s : Game.state)
              (m : Game.move) ((a, b) : alphabeta) : value = 
              case abmoves of
                  Seq.Nil => Seq.mapreduce
                             (fn x => F (d-1) (Game.make_move(s, x)) (a, b))
                             (Step(m, valueOf(a)))
                             (fn (x,y) => case valuecmp(x,y) of
                                              LESS => x
                                             | _ => y)
                             mmmoves
                | Seq.Cons(x, xs) =>
                  let
                    val new_b = F (d-1) (Game.make_move(s, x)) (a, b)
                  in
                    case (valuecmp(new_b, a),
                          valuecmp(new_b, b)) of
                        (GREATER, GREATER) => 
                        updateB d (Seq.showl(xs)) mmmoves s m (a, b)
                      | (GREATER, _) => 
                        updateB d (Seq.showl(xs)) mmmoves s x (a, new_b)
                      | _ => Step(x, Game.Est.minnie_wins)
                  end

  (* Task 6.4 *)
  (* next_move : Game.state -> Game.move *)
  (* REQUIRES: true *)
  (* ENSURES: next_move s evaluates to the best move the current
   * player can choose *)
  fun next_move (s : Game.state) : Game.move =
      let
      val best_move = case Game.player(s) of
                          Game.Minnie => G Settings.search_depth s initialAB
                        | Game.Maxie => F Settings.search_depth s initialAB
      in
        case best_move of
            Step(move, score) => move
          | _ => raise Fail "Leaf"
      end

end
