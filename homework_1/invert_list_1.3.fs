let invert ls =
    let length = List.length ls
    let rec invert list invertedList i =
        if i = length then
            invertedList
        else
            invert (List.tail list) (List.head list :: invertedList) (i + 1)
    invert ls [] 0

let list_1 = [1; 2; 3]
let list_2 = [(1,"a"); (2,"b"); (3,"c")]
let inverted_list_1 = invert list_1
let inverted_list_2 = invert list_2
