module sinAverage
    let findSinAverage list =
        let floatLength = List.length list |> float
        if floatLength < 1.0 then 0.0
        else
            let rec findAverage list (acc : float) =
                match list with
                | [] -> acc / floatLength
                | head :: tail -> findAverage tail (acc + sin head)
            findAverage list 0.0
