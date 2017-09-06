(* these are actually dependent on the particular implementation of scalars *)
signature SCALARCORE =
sig
  type scalar

  val plus : scalar * scalar -> scalar (* plus(s1,s2) means s1 + s2 *)
  val minus :  scalar * scalar -> scalar (* minus(s1,s2) means s1 - s2 *)
  val times : scalar * scalar -> scalar (* times(s1,s2) means s1 * s2 *)
  val divide :  scalar * scalar -> scalar (* divide(s1,s2) means s1 / s2 *)

  val compare : scalar * scalar -> order (* compare two scalars *)
  val fromRatio : IntInf.int * IntInf.int -> scalar
  val toString : scalar -> string
end

(* these guys are all derived forms, so let's save code and derive them *)
signature SCALAR =
sig
  include SCALARCORE

  val negate : scalar -> scalar

  val gte : scalar * scalar -> bool
  val lte : scalar * scalar -> bool
  val eq : scalar * scalar -> bool
  val min : scalar * scalar -> scalar
  val max : scalar * scalar -> scalar

  val invert : scalar -> scalar

  val zero : scalar
  val one : scalar
  val fromInt : IntInf.int -> scalar
  val pow : scalar * int -> scalar
end

(* see makescalar.sml for the derivations of the derived forms *)
