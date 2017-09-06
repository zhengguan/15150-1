signature GRAPHPROBLEMS =
sig
  type vertex = int
  type edge = vertex * vertex
  type graph = vertex Seq.seq Seq.seq

  val to_list : 'a Seq.seq -> 'a list

  val get_neighbors : graph -> vertex -> vertex Seq.seq
  val from_edge_seq : edge Seq.seq -> int -> vertex Seq.seq Seq.seq
  val is_undirected : graph -> bool
end

