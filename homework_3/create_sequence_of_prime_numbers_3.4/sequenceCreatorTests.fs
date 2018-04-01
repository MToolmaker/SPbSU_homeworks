module ``createSequence tests`` =
    open NUnit.Framework
    open FsUnit
    open sequenceCreator
    [<Test>]
    let ``First element of sequence transformed to list must be [2]`` () =
        Seq.toList(Seq.take 1 (createSequence())) |> should equal [2]
    
    [<Test>]
    let ``First 2 elements of sequence transformed to list must be [2; 3]`` () =
        Seq.toList(Seq.take 2 (createSequence())) |> should equal [2; 3]
    
    [<Test>]
    let ``First 5 elements of sequence transformed to list must be [2; 3; 5; 7; 11]`` () =
        Seq.toList(Seq.take 5 (createSequence())) |> should equal [2; 3; 5; 7; 11]

    [<Test>]
    let ``First 8 elements of sequence transformed to list must be [2; 3; 5; 7; 11; 13; 17; 19]`` () =
        Seq.toList(Seq.take 8 (createSequence())) |> should equal [2; 3; 5; 7; 11; 13; 17; 19]
