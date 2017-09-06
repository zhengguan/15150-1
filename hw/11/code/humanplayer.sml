(* The HumanPlayer just gets moves from what the user types in.
 * It is a functor that takes a game so that it knows how to
 * represent moves and the game state and knows how to find out if
 * a move is legal and whose turn it is.
 * We've written this one for you so you can use it to test your game.
 *)
functor HumanPlayer (G : GAME) : PLAYER =
struct
    structure Game = G

    fun next_move state =
      let val () =
          (print ((case (Game.player state) of
                       Game.Maxie => "Maxie"
                     | Game.Minnie => "Minnie")
                  ^ ", please type your move: "))
      in
        case TextIO.inputLine TextIO.stdIn of
            NONE => raise Fail "Failed to read a line of input from stdin"
          | SOME input =>
                let val input =
                    (* eat the newline character from the end of input *)
                    String.substring(input, 0, String.size(input) - 1)
                in
                    case Game.parse_move state input of
                        SOME m => m
                      | NONE =>
                            let val () = print ("Bad move for this state: "
                                                ^ input ^ "\n")
                            in 
                                next_move state 
                            end
                end
      end
end
