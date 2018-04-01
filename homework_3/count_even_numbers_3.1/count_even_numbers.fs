module count_even_numbers
    let foldEvenNumbers list = List.fold (fun acc x -> if x % 2 = 0 then acc + 1 else acc) 0 list
    let filterCountEvenNumbers list = list |> List.filter (fun x -> x % 2 = 0) |> List.length
    let mapCountEvenNumbers list = List.map (fun x -> if (x % 2) <> 0 then 0 else 1) list |> List.sum
