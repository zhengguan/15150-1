signature SEQUENCECORE =
sig
  type 'a seq

  exception Range of string

  (* length <x_0, ..., x_(n-1)> == n *)
  val length : 'a seq -> int
  (* nth i <x_0, ..., x_(n-1)> == x_i if 0 <= i < n, raises Range otherwise *)
  val nth    : int -> 'a seq -> 'a

  (* tabulate f n == <f 0, ..., f n-1> *)
  val tabulate : (int -> 'a) -> int -> 'a seq
  (* filter p <x_0, ..., x_(n-1)> == <x_i | p x_i == true>
   * that is, filter p s computes the sequence of all elements si of s such that
   * p si == true, in the original order. *)
  val filter : ('a -> bool) -> 'a seq -> 'a seq

  (* map f <x_0, ..., x_(n-1)> == <f x_0, ..., f x_(n-1)> *)
  val map : ('a -> 'b) -> 'a seq -> 'b seq
  (* reduce op b <x_0, ..., x_(n-1)> == x_0 op x2 ... op x_(n-1),
   * that is, reduce applies the given function between all elements of the
   * input sequence, using b as the base case. *)
  val reduce : (('a * 'a) -> 'a) -> 'a -> 'a seq -> 'a

  datatype 'a lview = Nil | Cons of 'a * 'a seq
  datatype 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  val showl : 'a seq -> 'a lview
  val hidel : 'a lview -> 'a seq

  val showt : 'a seq -> 'a tview
  val hidet : 'a tview -> 'a seq
end
