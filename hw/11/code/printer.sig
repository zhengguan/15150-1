signature Printer =
sig
    val nDups : char -> int -> string

    val pad : int -> string -> string

    val padAll : string Seq.seq -> string Seq.seq

    val labelAll : string Seq.seq -> string Seq.seq
end
