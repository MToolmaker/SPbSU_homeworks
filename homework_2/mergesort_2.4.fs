let mergesort list =
    let rec split list left right = 
        match list with
        | [] -> (left, right)
        | [a] -> (a::left, right)
        | a::b::tail -> split tail (a::left) (b::right)

    let merge left right =
        let rec merge left right acc =
            match (left, right) with
            | ([], []) -> acc
            | (a::tail, []) -> merge tail [] (a::acc)
            | ([], b::tail) -> merge [] tail (b::acc)
            | (a::leftTail, b::rightTail) ->
                if (a < b) then
                    merge leftTail right (a::acc)
                else
                    merge left rightTail (b::acc)
            
        merge left right [] |> List.rev

    let rec mergesort list = 
        match list with
        | [] -> []
        | x::[] -> list
        | _ ->
              let (left, right) = split list [] []
              let sortedLeft = mergesort left
              let sortedRight = mergesort right
              merge sortedLeft sortedRight   
    mergesort list

module ``Mergesort tests`` =
    open NUnit.Framework
    open FsUnit
    [<Test>]
    let  ``mergesort [5; 4; 3; 2; 1] must return [1; 2; 3; 4; 5]`` () = 
        mergesort [5; 4; 3; 2; 1] |> should equal [1; 2; 3; 4; 5]

    [<Test>]
    let  ``mergesort [1; 2; 3; 4; 5] must return [1; 2; 3; 4; 5]`` () = 
        mergesort [1; 2; 3; 4; 5] |> should equal [1; 2; 3; 4; 5]

    [<Test>]
    let  ``mergesort [] must return []`` () = 
        mergesort [] |> should equal []
    
    [<Test>]
    let  ``mergesort [1..1000000] must return [1..1000000]`` () = 
        mergesort [1..1000000] |> should equal [1..1000000]

    let reversedTestList = List.init 1000000 (fun x -> 1000000-x)
    [<Test>]
    let  ``mergesort [1000000..1] must return [1..1000000]`` () = 
        mergesort reversedTestList |> should equal [1..1000000]

    let testList2 = [for i in 1..1000 -> i % 2]
    let zeros = [for i in 1..500 -> 0]
    let ones = [for i in 501..1000 -> 1] 
    let expected = zeros @ ones

    [<Test>]
    let  ``mergesort [1; 0; 1; ...] must return [0; 0; ..; 1; 1; ..]`` () = 
        mergesort testList2 |> should equal expected

