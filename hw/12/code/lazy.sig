signature LAZY =
sig
  
  datatype lazylist = datatype LazyList.lazylist

  val lazy_merge : ('a * 'a -> order) -> 'a lazylist -> 'a lazylist -> 'a lazylist
  val combine : ('a * 'a -> order) -> 'a lazylist lazylist -> 'a lazylist
  val pairs : (int * int) lazylist

end