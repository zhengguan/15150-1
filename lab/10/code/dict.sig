signature DICT =
sig
  
  type 'a dict
  type key

  (* return the number of keys in the dictionary *)
  val size : 'a dict -> int

  (* returns SOME v if d contains the mapping (k -> v) and NONE otherwise *)
  val find : 'a dict -> key -> 'a option

  (* returns a dict with the key value pair (k -> v) if k is not already in dict 
   * if k is already in dict, call f to determine the key's value*)
  val insert : ('a * 'a -> 'a) -> (key * 'a) -> 'a dict -> 'a dict
  
  (* returns a dict with k removed *)
  val delete : key -> 'a dict -> 'a dict

  (* returns the empty dict *)
  val empty : unit -> 'a dict

  (* returns a singleton dict with only (k -> v) *)
  val singleton : key * 'a -> 'a dict

  (* returns dict with f mapped to all values *)
  val map : ('a -> 'b) -> 'a dict -> 'b dict

  (* returns dict with all values such that f v = true *)
  val filter  : ('a -> bool) -> 'a dict -> 'a dict

  (* returns dict with all keys such that f k = true *)
  val filterk : (key -> bool) -> 'a dict -> 'a dict

  (* returns the dict that contains all keys in both d1 and d2
   * if there are duplicate keys, call f to determine the key's value *)
  val merge   : ('a * 'a -> 'a) -> ('a dict * 'a dict) -> 'a dict

  (* returns the list representation of a dict [(k1,v1), ,(kn,vn)] *)
  val toList : 'a dict -> (key * 'a) list

  (* returns the dict given a list representation *)
  val fromList : (key * 'a) list -> 'a dict

end
