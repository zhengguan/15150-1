functor MMTwo(G : GAME) = MiniMax(struct structure G = G
                                         val search_depth = 2
                                  end)

functor MMFour(G : GAME) = MiniMax(struct structure G = G
                                         val search_depth = 4
                                  end)

functor ABFour(G : GAME) = AlphaBeta(struct structure G = G
                                            val search_depth = 4
                                     end)

functor ABSix(G : GAME) = AlphaBeta(struct structure G = G
                                           val search_depth = 6
                                     end)

functor Jam(G : GAME) = Jamboree(struct structure G = G
                                        val search_depth = 6
                                        val prune_percentage = 0.5
                                 end)


structure Risk_HvMM = Referee(struct
                                 structure Maxie  = HumanPlayer(RL2)
                                 structure Minnie = MMTwo(RL2)
                              end)


(* Task 5.4 *)
structure Risk_HvAB = Referee(struct
                                 structure Maxie  = HumanPlayer(RL2)
                                 structure Minnie = ABFour(RL2)
                              end)

                             
structure Risk_HvJam = Referee(struct
                                 structure Maxie  = HumanPlayer(RL2)
                                 structure Minnie = Jam(RL2)
                              end)
