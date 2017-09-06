structure Estimate : EST =
struct
    type est = int
    val minnie_wins = valOf (Int.minInt)
    val maxie_wins = valOf (Int.maxInt)
    val draw = 0
    fun guess x = x

    val compare = Int.compare

    fun toString n =
      case (n = minnie_wins , n = maxie_wins , n = draw) of
          (true, _, _) => "Minnie wins!"
        | (_, true, _) => "Maxie wins!"
        | (_, _, true) => "It's a draw!"
        | _ => "Guess: " ^ Int.toString n
end
