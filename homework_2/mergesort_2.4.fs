let rec mergesort list =
    let rec split list left right = 
        match list with
        | [] -> (left, right)
        | [a] -> (a::left, right)
        | a::b::tail -> split tail (a::left) (b::right)

    let rec merge left right =
        match (left, right) with
        | ([], right) -> right
        | (left, []) -> left
        | (a::left_tail, b::right_tail) ->
            if (a < b) then
                a::(merge left_tail right) 
            else
                b::(merge left right_tail)

    let mergesort list = 
        match list with
        | [] -> []
        | x::[] -> list
        | _ ->
              let (left, right) = split list [] []
              let sorted_left = mergesort left
              let sorted_right = mergesort right
              merge sorted_left sorted_right
              
    mergesort list

// Must be [1;2;3;4;5]
mergesort [5;4;3;2;1]
// Must be []
mergesort []
// Must be [1]
mergesort [1]
// Must be [1;2;3;4;5]
mergesort [5;3;4;1;2]
