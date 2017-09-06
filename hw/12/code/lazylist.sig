signature LAZY_LIST =
sig

  datatype 'a lazylist = Cons of 'a * (unit -> 'a lazylist) 

  val show : int -> 'a lazylist -> 'a list
  val append : 'a list -> (unit -> 'a lazylist) -> 'a lazylist
  val repeat : 'a list -> 'a lazylist
  val zip : 'a lazylist -> 'b lazylist -> ('a * 'b) lazylist
  val iterate : ('a -> 'a) -> 'a -> 'a lazylist 
  val compare : ('a * 'a -> bool) -> int -> 'a lazylist -> 'a lazylist -> bool
  val match : ('a * 'a -> bool) -> 'a list -> 'a lazylist -> bool

end
