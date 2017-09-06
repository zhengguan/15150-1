functor MiniMax (Settings : sig
                               structure G : GAME
                               val search_depth : int
                            end
                 ) : PLAYER =
struct
    structure Game = Settings.G

    type edge = (Game.move * Game.est)
    fun valueOf ((_,value) : edge) = value
    fun moveOf ((move,_) : edge) = move

    fun max ((m1,v1) : edge, (m2,v2) : edge) : edge =
        case Game.Est.compare (v1, v2) of
          LESS => (m2, v2)
        | _ => (m1, v1)

    fun min ((m1,v1) : edge, (m2,v2) : edge) : edge =
        case Game.Est.compare (v1, v2) of
          GREATER => (m2, v2)
        | _ => (m1, v1)

    fun vmax (v1, v2) : Game.est =
        case Game.Est.compare (v1, v2) of
          LESS => v2
        | _ => v1

    fun vmin (v1, v2) : Game.est =
        case Game.Est.compare (v1, v2) of
          GREATER => v2
        | _ => v1

    fun score (s : Game.state) : Game.est =
        case Game.status s of
          Game.Over (Game.Draw) => Game.Est.draw
        | Game.Over (Game.Winner (Game.Minnie)) => Game.Est.minnie_wins
        | Game.Over (Game.Winner (Game.Maxie)) => Game.Est.maxie_wins
        | Game.In_play => raise Fail "No moves available"

    fun F (d : int) (s : Game.state) : Game.est =
        let
          val moves = Game.moves s
        in
          if SeqUtils.null moves then score s else
          if d = 0 then Game.estimate s else
            SeqUtils.reduce1 vmax
            (Seq.map (fn mv => G (d - 1) (Game.make_move (s, mv))) (moves))
        end

    and G (d : int) (s : Game.state) : Game.est =
        let
          val moves = Game.moves s
        in
          if SeqUtils.null moves then score s else
          if d = 0 then Game.estimate s else
            SeqUtils.reduce1 vmin
            (Seq.map (fn mv => F (d - 1) (Game.make_move (s, mv))) (moves))
        end

    fun next_move (s : Game.state) : Game.move =
        let
          val moves = Game.moves s
         in
          case Game.player s of
            Game.Minnie =>
            moveOf (SeqUtils.reduce1 min
                   (Seq.map (fn mv => (mv, F (Settings.search_depth - 1)
                                       (Game.make_move (s, mv)))) moves))
          | Game.Maxie =>
            moveOf (SeqUtils.reduce1 max
                   (Seq.map (fn mv => (mv, G (Settings.search_depth - 1)
                                       (Game.make_move (s, mv)))) moves))
        end
end
