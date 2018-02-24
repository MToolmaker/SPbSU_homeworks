let firstOccurrence list n =
    let length = List.length list
    let rec firstOccurrence list i =
        if i = length then
            None
        else if List.head list = n then
            Some(i)
        else
            firstOccurrence (List.tail list) (i + 1)
    firstOccurrence list 0

let first = firstOccurrence [0; 1; 3; 2] 2
let second = firstOccurrence [] 3
let third = firstOccurrence [5; 8; 9; 10] 8
