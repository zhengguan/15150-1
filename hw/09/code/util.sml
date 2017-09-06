structure Util : UTIL =
struct
    (* first component is (char 0 , ... char (i - 1))
       position i is included in the second component *)
    fun splitAt (s , i) =
        SOME (String.extract (s , 0, SOME i), String.extract (s , i , NONE))
        handle Subscript => NONE

    fun peelOff (this : string, from : string) : string option =
        case splitAt (from , String.size this) of
            SOME (first , rest) =>
                (case first = this of
                     true => SOME rest
                   | false => NONE)
          | NONE => NONE

    fun viewFirst (s : string) : (char * string) option =
        case String.size s of
            0 => NONE
          | _ => SOME (String.sub (s,0) , String.extract (s , 1, NONE))

    fun digit d =
        case d of
            #"0" => true
          | #"1" => true
          | #"2" => true
          | #"3" => true
          | #"4" => true
          | #"5" => true
          | #"6" => true
          | #"7" => true
          | #"8" => true
          | #"9" => true
          | _ => false

    fun peelInt (from : string) : (int * string) option =
        let
            fun peel (from : string) : (char list * string) option =
                case viewFirst from of
                    NONE => NONE
                  | SOME (first , s) =>
                        (case digit first of
                             false => NONE
                           | true => (case peel s of
                                          NONE => SOME ([first] , s)
                                        | SOME (cs , s) => SOME (first :: cs , s)))
        in
            case viewFirst from of
                NONE => NONE
              | SOME (c , s) =>
                    let
                        val (neg,rest) = case c of #"~" => ("~",s) | _ => (" ",from)
                    in
                        case peel rest of
                            SOME (cs , s) =>
                                (case Int.fromString (neg ^ String.implode cs) of
                                     SOME i => SOME (i, s)
                                   | NONE => NONE)
                          | NONE => NONE
                    end
        end
end
