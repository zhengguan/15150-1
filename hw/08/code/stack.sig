signature STACK = 
sig
  type stacktype
  type stack
  
  exception StackUnderflow
  val empty : unit -> stack
  val push  : (stack * stacktype) -> stack
  val pop   : stack -> (stacktype * stack)
  val append : (stack * stack) -> stack
  val find : (stack * (stacktype -> bool)) -> stack option
  val flip : stack -> stack

end	 