functor AlphaBeta (Settings : sig
                                 structure G : GAME
                                 val search_depth : int
                             end)
        : PLAYER where type Game.move = Settings.G.move
                 where type Game.state = Settings.G.state =
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

  (* valueOf : value -> Game.est
   *   Get the estimate associated with the value *)
  fun valueOf (Step (_, e)) = e
    | valueOf (Leaf e) = e

  (* moveOf : value -> Game.move
   *    Get the move associated with a Step *)
  fun moveOf (Step (m, _)) = m
    | moveOf _ = raise Fail "no moves!"

  (* valuecmp : value -> order
   *    Compare two values to see which represents a larger estimate *)
  fun valuecmp (x, y) = Game.Est.compare (valueOf x, valueOf y)

  (* maxalpha : value * value -> value
   *     Get the maximum of two values *)
  fun maxalpha (x, y) =
      case (valuecmp (x,y), (x, y)) of
        (GREATER, _) => x
      | (LESS, _) => y
      | (EQUAL, (Step _, Leaf _)) => x
      | (EQUAL, (Leaf _, Step _)) => y
      | _ => x

  (* minbeta : value * value -> value
   *      Get the minimum of two values *)
  fun minbeta (x, y) =
      case (valuecmp (x,y), (x, y)) of
        (GREATER, _) => y
      | (LESS, _) => x
      | (EQUAL, (Step _, Leaf _)) => x
      | (EQUAL, (Leaf _, Step _)) => y
      | _ => x


  (* Task 5.1 *)
  val initialAB : alphabeta = (Leaf(Game.Est.minnie_wins), Leaf(Game.Est.maxie_wins))

  (* Task 5.2 *)
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
                                      val move_seq = Game.moves(s)
                                    in
                                      updateA d (Seq.showl(move_seq))
                                              s (Seq.nth 0 move_seq) (a, b)
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
                                      val move_seq = Game.moves(s)
                                    in
                                      updateB d (Seq.showl(move_seq))
                                              s (Seq.nth 0 move_seq) (a, b)
                                    end)
        | Game.Over(Game.Winner(Game.Minnie)) =>
          Leaf(Game.Est.minnie_wins)
        | Game.Over(Game.Winner(Game.Maxie)) =>
          Leaf(Game.Est.maxie_wins)
        | Game.Over(Game.Draw) => Leaf(Game.Est.draw)
               
  (* updateB : int -> Game.move Seq.lview -> Game.state ->
   * Game.move -> alphabeta *) 
  (* REQUIRES: d > 0, status s is In_Play, Game.moves s evaluates to a
   * non-empty sequence of valid moves that includes m *)
  (* ENSURES: Computes the best possible game result given a game state s
   * at a depth d for Maxie *)                     
  and updateA (d : int) (move_view : Game.move Seq.lview) (s : Game.state)
              (m : Game.move) ((a, b) : alphabeta) : value =
              case move_view of
                  Seq.Nil => Step(m, valueOf(a))
                | Seq.Cons(x, xs) =>
                  let
                    val new_a = G (d-1) (Game.make_move(s, x)) (a, b)
                  in
                    case (valuecmp(new_a, b),
                          valuecmp(new_a, a)) of
                        (LESS, LESS) => 
                        updateA d (Seq.showl(xs)) s m (a, b)
                      | (LESS, _) => 
                        updateA d (Seq.showl(xs)) s x (new_a, b)
                      | _ => Step(x, Game.Est.maxie_wins)
                  end
                  
  (* updateB : int -> Game.move Seq.lview -> Game.state ->
   * Game.move -> alphabeta *) 
  (* REQUIRES: d > 0, status s is In_Play, Game.moves s evaluates to a
   * non-empty sequence of valid moves that includes m *)
  (* ENSURES: Computes the best possible game result given a game state s
   * at a depth d for Minnie *)                              
  and updateB (d : int) (move_view : Game.move Seq.lview) (s : Game.state)
              (m : Game.move) ((a, b) : alphabeta) : value =
              case move_view of
                  Seq.Nil => Step(m, valueOf(b))
                | Seq.Cons(x, xs) =>
                  let
                    val new_b = F (d-1) (Game.make_move(s, x)) (a, b)
                  in
                    case (valuecmp(new_b, a),
                          valuecmp(new_b, b)) of
                        (GREATER, GREATER) =>
                        updateB d (Seq.showl(xs)) s m (a, b)
                      | (GREATER, _) => 
                        updateB d (Seq.showl(xs)) s x (a, new_b)
                      | _ => Step(x, Game.Est.minnie_wins)
                  end

  (* Task 5.3 *)
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
