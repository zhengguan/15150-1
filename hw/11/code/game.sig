signature GAME =
sig
    datatype player = Minnie | Maxie

    datatype outcome = Winner of player | Draw
    datatype status = Over of outcome | In_play

    structure Est : EST

    type state (* state of the game; e.g. board and player *)
    type move (* moves *)

    (* views of the state: *)
    (* assuming state is not over, generates non-empty seq of valid moves *)
    val moves : state -> move Seq.seq
    val status : state -> status
    val player : state -> player

    (* initial state and transitions: *)
    val start : state
    (* assumes move is valid in that state *)
    val make_move : (state * move) -> state

    (* The sign of a guess is absolute, not relative to whose turn it is:
       negative values are better for Minnie
       and positive values are better for Maxie. *)
    type est = Est.est
    (* estimate the value of the state, which is assumed to be In_play *)
    val estimate : state -> est

    val move_to_string : move -> string
    val state_to_string : state -> string

    (* ensures move is valid in that state;
       string is single line, and *not* newline terminated *)
    val parse_move : state -> string -> move option
end

