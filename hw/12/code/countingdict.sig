signature COUNTING_DICT =
sig
  structure Key : ORDERED
  type 'a dict

  val build : (Key.t * 'a) list -> 'a dict
  val lookup : 'a dict -> Key.t -> 'a option
  val hits : 'a dict -> Key.t -> int
end
