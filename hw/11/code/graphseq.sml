structure Graphs : GRAPHPROBLEMS =
struct
  open Seq

  (* Note: complete methodology for all functions you write. *)
  type vertex = int
  type edge = vertex * vertex
  type graph = vertex seq seq

  exception not_in_graph

  (* Task 2.1 *)
  (* to_list : ’a seq -> ’a list *)
  (* REQUIRES: true *)
  (* ENSURES: to_list S converts the sequence S into a list *)
  fun to_list (S : 'a seq) : 'a list =
      case showl S of 
            Nil => []
          | Cons (x, S') => x::(to_list S')

  (* Task 3.1 *)
  (* get_neighbors : vertex seq seq -> vertex -> vertex seq *)
  (* REQUIRES: G is a undirected, connected graph *)
  (* ENSURES: get_neighbors G u evaluates to a sequence containing
   * only the neighbors of u in G *)
  fun get_neighbors (G : graph) (u : vertex) : vertex seq =
      if (u < length G)
        then (nth u G)
        else raise not_in_graph

  (* Task 3.2 *)
  (* from_edge : (vertex * vertex) seq -> int -> vertex seq seq *)
  (* REQUIRES: E is a valid list of edges for a grpah of n vertices *)
  (* ENSURES: from_edge E n evalutes to the adjacency list representation
   * of the graph with n vertices and edges E *)
  fun from_edge_seq (E : edge seq) (n : int) : vertex seq seq =
      tabulate (fn i => map (fn (x,y) => y)
                            (filter (fn (x,y) => x = i) E))
               n

  (* Task 3.3 *)
  (* is_undirected : graph -> bool *)
  (* REQUIRES: true *)
  (* ENSURES: returns true if all edges in a graph G are undirected and
   * false otherwise *)
  fun is_undirected (G : graph) : bool =
      let
        val exists = fn (x, S) => List.exists (fn y => y = x) (to_list(S))
      in
        reduce (fn (u,v) => u andalso v) true
               (tabulate (fn i => mapreduce
                                  (fn x => exists (i, (nth x G)))
                                  true (fn (u,v) => u andalso v) 
                                  (nth i G))
                         (length G))
      end

end

structure TestGraphs =
struct
  open Seq
  open Graphs

  fun fromList ([] : 'a list) : 'a seq = empty ()
    | fromList (x::xs) = cons x (fromList xs)

  (* Tests for to_list *)
  val [1,2,3] = to_list(fromList([1,2,3]))
  val [0,5,3,1] = to_list(fromList([0,5,3,1]))
  val [2,2,2] = to_list(fromList([2,2,2]))
  val [1] = to_list(fromList([1]))
  
  (* Tests for get_neighbors *)
  val G = fromList([fromList([1]), fromList([0,3]),
                      fromList([3]), fromList([1,2])])
  val [1] = to_list(get_neighbors G 0)
  val [0,3] = to_list(get_neighbors G 1)
  val [3] = to_list(get_neighbors G 2)
  val [1,2] = to_list(get_neighbors G 3)
  
  (* Tests for from_edges *)
  val E = fromList([(0,1), (1,0), (1,2), (2,1)])
  val [[1], [0,2], [1]] = to_list(map (fn x => to_list(x))
                                     (from_edge_seq E 3))
                  
  (* Tests for is_undirected *)
  val G1 = fromList([fromList([1]), fromList([0,3]),
                     fromList([3]), fromList([1,2])])
  val G2 = fromList([fromList([1]), fromList([2]),
                     fromList([1,3]), fromList([1,2])])
  val true = is_undirected(G1)
  val false = is_undirected(G2)
  
end
