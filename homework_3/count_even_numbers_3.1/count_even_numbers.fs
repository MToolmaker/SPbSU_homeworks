module countEvenNumbers
    let foldCountEvenNumbers list = if list = [] then None else Some(List.fold (fun acc x -> if x % 2 = 0 then acc + 1 else acc) 0 list)
    let filterCountEvenNumbers list = if list = [] then None else Some(list |> List.filter (fun x -> x % 2 = 0) |> List.length)
    let mapCountEvenNumbers list = if list = [] then None else Some(List.map (fun x -> if (x % 2) <> 0 then 0 else 1) list |> List.sum)
