type dir = 
| North
| South
| East
| West
type path = dir list

let dist path = 
    let rec displacement path nDisplacement eDisplacement = 
        match path with
        | [] -> 
            if nDisplacement = 0 then float_of_int (abs eDisplacement)
            else if eDisplacement = 0 then float_of_int (abs nDisplacement)
            else sqrt (float_of_int (nDisplacement * nDisplacement + eDisplacement * eDisplacement))
        | North :: rest -> displacement rest (nDisplacement + 1) eDisplacement
        | South :: rest -> displacement rest (nDisplacement - 1) eDisplacement
        | East :: rest -> displacement rest nDisplacement (eDisplacement + 1)
        | West :: rest -> displacement rest nDisplacement (eDisplacement - 1)
    in displacement path 0 0
