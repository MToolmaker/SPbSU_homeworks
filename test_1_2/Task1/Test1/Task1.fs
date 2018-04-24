module Task1
    let supermap list =
        list |> List.map (fun x -> [sin x; cos x]) |> List.concat


