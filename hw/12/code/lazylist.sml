structure LazyList : LAZY_LIST = 
struct 
  datatype 'a lazylist = Cons of 'a * (unit -> 'a lazylist)
  
  (* show : int -> 'a lazylist -> 'a list *)
  fun show 0 _ = [ ]
    | show n (Cons(x, h)) = x :: show (n-1) (h ())
  
  (* append : 'a list -> (unit -> 'a lazylist) -> 'a lazylist *)
  fun append [] h = h ()
    | append (x :: xs) h = Cons(x, fn () => append xs h)

  (* repeat : 'a list -> 'a lazylist *)
  (* REQUIRES xs not empty *)
  (* ENSURES repeat(xs) terminates *)
  fun repeat xs = append xs (fn ( ) => repeat xs)

  (* zip : 'a lazylist -> 'a lazylist -> 'a lazylist *)
  fun zip (Cons(x1, f1)) (Cons(x2, f2)) = 
      Cons((x1, x2), fn () => zip (f1 ()) (f2 ()))

  (* iterate : ('a -> 'a) -> 'a -> 'a lazylist *)
  fun iterate next z = Cons(z, fn () => iterate next (next z))

  (* compare : ('a * 'a -> bool) -> int -> 'a lazylist -> 'a lazylist -> bool *)
  fun compare eq 0 _ _ = true
    | compare eq n (Cons(x1, f1)) (Cons(x2, f2)) =
      eq (x1, x2) andalso compare eq (n - 1) (f1 ()) (f2 ())

  (* match : ('a * 'a -> bool) -> 'a list -> 'a lazylist -> bool *)
  fun match eq [] _ = true
    | match eq (x1 :: xs) (Cons(x2, f)) = 
      eq (x1, x2) andalso match eq xs (f ())

end