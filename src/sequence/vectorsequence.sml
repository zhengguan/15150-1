structure VectorSeq : SEQUENCE = struct
  structure VS = DeriveSequence (VectorCore)
  open VS

  (* TODO: optimized versions of split, take, and drop. *)
end
