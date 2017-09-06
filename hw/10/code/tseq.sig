signature TSEQ =
sig
    type 'a seq
    exception Range
    val length : 'a seq -> int
    val nth : int -> 'a seq -> 'a
    val tabulate : (int -> 'a) -> int -> 'a seq
end
