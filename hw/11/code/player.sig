signature PLAYER =
sig
    structure Game : GAME

    (* assumes game is In_play *)
    val next_move : Game.state -> Game.move
end

