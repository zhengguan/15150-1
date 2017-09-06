signature EST =
sig
    type est

    (* Values that represent the best possible scores,
       for Minnie and Maxie as well as the draw score *)
    val minnie_wins : est
    val maxie_wins : est
    val draw : est

    (* Given a value, guesses the score *)
    val guess : est -> est

    (* Compares two estimated values and returns an order *)
    val compare : (est * est) -> order

    (* Utility function for printing estimated values *)
    val toString : est -> string
end

