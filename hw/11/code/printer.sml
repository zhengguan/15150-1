structure P : Printer =
struct
    fun nDups c n =
      String.implode (List.tabulate (n, fn i => c))

    fun pad l s =
      let val sl = String.size s
          val padding = l - sl
      in if padding < 0 then s
         else s ^ (nDups #" " padding)
      end

    fun padAll ss =
      let val ml = Seq.mapreduce String.size 0 Int.max ss
      in Seq.map (pad ml) ss
      end

    fun labelAll ss =
      Seq.tabulate (fn i => "(" ^ (Int.toString i) ^ ") " ^ (Seq.nth i ss))
                   (Seq.length ss)
end
