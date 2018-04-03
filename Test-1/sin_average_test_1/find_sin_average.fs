module sinAverage
    let findSinAverage list =
        let floatLength = List.length list |> float
        let rec findAverage list (acc : float) =
            match list with
            | [] -> acc / floatLength
            | head :: tail -> findAverage tail (acc + head)
        findAverage list 0.0 |> sin
