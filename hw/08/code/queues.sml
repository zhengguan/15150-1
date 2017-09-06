(* code for the proof task; you don't need to modify or submit this file *)

signature QUEUE=
sig
   type queue
   val emp : queue
   val ins : int * queue -> queue
   val rem : queue -> (int * queue) option
end

structure LQ : QUEUE =
struct
     type queue = int list

     val emp = []
     fun ins (n , l) = l @ [n]
     fun rem l =
        case l
         of [] => NONE
          | x::xs => SOME (x, xs)
end

structure LLQ : QUEUE =
struct
     type queue = (int list) * (int list)

     val emp = ([],[])
     fun ins (n, (front,back)) = (front,n :: back)

     fun rem  (front,back) =
        case (front,back)
         of ([],[]) => NONE
          | (x::xs,_) => SOME (x, (xs,back))
          | ([],_) => rem (rev back,[])
end
