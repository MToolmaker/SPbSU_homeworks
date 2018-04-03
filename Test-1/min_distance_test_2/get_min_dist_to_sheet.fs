module minDistance
    type Tree<'a> = 
        | Empty
        | Tree of 'a * Tree<'a> * Tree<'a>
    
    let getMinDistToSheet tree =
        if tree = Empty then None
        else
            let rec getMinDistToSheet tree i =
                match tree with
                | Empty -> i
                | Tree(root, left, right) -> min (getMinDistToSheet left (i + 1)) (getMinDistToSheet right (i + 1))
            Some(getMinDistToSheet tree 0)
