signature SEQUENCE =
sig
  include SEQUENCECORE

  (* mapreduce l e n s == reduce n e (map l s) *)
  val mapreduce : ('a -> 'b) -> 'b -> ('b * 'b -> 'b) -> 'a seq -> 'b
  (* Intuitively toString computes the string representation of a sequence
   * in the same style as the string for a list, using the given function to
   * convert each element to a string. More precisely:
   * toString elmToStr <x_0, ..., x_(n-1)> == "[" ^ elmToStr x_0 ^ ", " ... ^ elmToStr x_(n-1) ^ "]". *)
  val toString : ('a -> string) -> 'a seq -> string

  (* repeat n x == <x_0, ..., x_(n-1)> such that all x_i == x *)
  val repeat  : int -> 'a -> 'a seq
  (* zip (<x_0, ..., x_(n-1)>, <y_0, ..., y_(m-1)>) == <(x_0, y_0), ..., (x_k, y_k)> where
   * k = min(n,m). That is, zip truncates the longer sequence if needed *)
  val zip     : ('a seq * 'b seq) -> ('a * 'b) seq
  (* flatten ss == reduce append <> ss (the sequence analog of concat).
   * See below for the specification of append. *)
  val flatten : 'a seq seq -> 'a seq

  (* split k <x_0,...x_(k-1),x_k...x_(n-1)> == (<x_0,...x_(k-1)>, <x_k,...x_(n-1)>)
     (so the left result has length k)
     if the sequence has at least k elements

     or raises Range otherwise
  *)
  val split   : int -> 'a seq -> 'a seq * 'a seq

  (* take k <x_0,...x_(k-1),x_k...x_(n-1)> == <x_0,...x_(k-1)>
     drop k <x_0,...x_(k-1),x_k...x_(n-1)> == <x_k,...x_(n-1)>
     if the sequence has at least k elements

     or raise Range otherwise
     *)
  val take    : int -> 'a seq -> 'a seq
  val drop    : int -> 'a seq -> 'a seq

  (* empty () == <> *)
  val empty : unit -> 'a seq
  (* cons x_0 <x_1, ..., x_(n-1)> == <x_0, ..., x_(n-1)> *)
  val cons  : 'a -> 'a seq -> 'a seq

  (* singleton x == <x> *)
  val singleton : 'a -> 'a seq
  (* append <x_0, ..., x_(n-1)> <y_0, ..., y_(m-1)> == <x_0, ..., x_(n-1), y_0, ..., y_(m-1)> *)
  val append    : 'a seq -> 'a seq -> 'a seq


  (* DRL, Spring 2012:
     made some interface-level changes to currying/argument order.
     these still need to get pushed through the HWs and labs.

     what changed:
     toString: swapped argument order and curried
     repeat: curried
     nth/split/take/drop: integer first
     append : curried
     cons: curried
   *)
end
