signature MAP =
sig
    (* Adjacency sequence representation of the game board *)
    val board : (string * int Seq.seq) Seq.seq
    (* Converts a sequence of node labels to string *)
    val board_to_string : string Seq.seq -> string

    (* Number of units that Minnie and Maxie start with *)
    val min_start : int
    val max_start : int

    (* Win condition: number of units to reach *)
    val max_units : int
    (* Constraint: maximum units on one territory *)
    val max_terri : int
end
