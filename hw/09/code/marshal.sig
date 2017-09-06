signature MARSHAL =
sig
    type t

    (* invariant: For all v, s: read (write v ^ s) == SOME (v , s) *)
    val write : t -> string
    val read : string -> (t * string) option
end
