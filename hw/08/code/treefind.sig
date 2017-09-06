signature TREEFIND =
sig
  datatype 'a ntree = Empty
                    | Node of 'a * 'a ntree list
  type 'a tree = 'a ntree
  val find : ('a -> bool) -> 'a tree -> 'a tree
end
