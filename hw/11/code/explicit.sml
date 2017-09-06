
structure GameTree =
struct

    datatype player = Minnie | Maxie

    datatype tree =
        Esti of string * int
      | Node of string * tree list

end

(* useful for testing search algorithms that use the estimator;
   unbounded complete search would loop.
 *)
functor ExplicitGame (A: sig
                             val tree : GameTree.tree
                         end) : GAME where type move = int =
struct

    open GameTree

    datatype outcome = Winner of player | Draw
    datatype status = Over of outcome | In_play

    structure Est = Estimate
    type est = Est.est

    type move = int

    datatype absstate = S of (tree * player)
    type state = absstate

    fun status (S t) = In_play

    fun moves (s as S (t, _)) =
        case t of
            Esti v => Seq.tabulate (fn x => x) 1
          | Node (_,succs) => Seq.tabulate (fn x => x) (List.length succs)

    fun player (S (_, p)) = p

    val start = S (A.tree, Maxie)

    fun make_move (s as S (t,p), i) =
        case t of
            Esti _ => raise Fail "called make_move on an Esti state"
          | Node (_,next) => S(List.nth (next,i), case p of Maxie => Minnie | Minnie => Maxie)

    (* estimate the value of the state, which is assumed to be In_play *)
    fun estimate (S(t,p)) =
        case t of
            Esti(s,v) => (print ("Estimating state " ^ s ^ "[" ^ Int.toString v ^ "]\n") ; v)
          | _ => raise Fail "called estimate on a non-estimate node"

    val move_to_string = Int.toString

    fun state_to_string (S(t,p)) = (case p of Maxie => "(Maxie," | Minnie => "(Minnie,") ^
        (case t of
            Esti(s,_) => s
          | Node(s,_) => s) ^ ")"

    fun parse_move s = raise Fail ""

end

(* run with search depth 2*)
structure HandoutSmall : GAME =
ExplicitGame(struct
                 open GameTree
                 val tree = Node ("a",
                                  [Node("b",
                                        [Esti("c", 3),Esti("d",6),Esti("e",~2)]),
                                   Node("f",
                                        [Esti("g", 6),Esti("h",4),Esti("i",10)]),
                                   Node("j",
                                        [Esti("k", 1),Esti("l",30),Esti("m",9)])])
             end)

(* run with search depth 4*)
structure HandoutBig : GAME =
ExplicitGame(struct
                 open GameTree
                 val tree = Node ("a",
                                  [Node("b",
                                        [Node("c",[Node("d",[Esti("e",3),Esti("f",5)]),
                                                   Node("g",[Esti("h",2),Esti("i",7)])]),
                                         Node("j",[Node("k",[Esti("l",10),Esti("m",4)])])]),
                                   Node("n",
                                        [Node("o",[Node("p",[Esti("q",2),Esti("r",7)])]),
                                         Node("s",[Node("t",[Esti("u",8),Esti("v",2)]),
                                                   Node("w",[Esti("x",4),Esti("y",6)])])])])
             end)

structure BrokenTree : GAME =
ExplicitGame(struct
  open GameTree
  val tree = Node ("a", [Node("b", [Node("c", [Esti("d",4)])]),
    Node("e", [Node ("f", [Esti("g",5)]),
      Node ("h", [Esti("i",6)])])])
  end)
