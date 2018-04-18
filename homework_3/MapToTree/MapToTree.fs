module MapToTree
    type Tree<'a> = 
    | Empty
    | Tree of 'a * Tree<'a> * Tree<'a>
    
    let rec map tree f =
        match tree with
        | Empty -> Empty
        | Tree(root, left, right) -> 
            Tree(f root, map left f, map right f)

