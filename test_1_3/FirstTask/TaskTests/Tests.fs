module Tests
    open NUnit.Framework
    open FsUnit
    open TaskModule

    [<Test>]
    let ``Take 1 element of seq``() =
        Seq.toList(Seq.take 1 (generateSeq())) |> should equal [1]
        
    [<Test>]
    let ``Take 2 elements of seq``() =
        Seq.toList(Seq.take 2 (generateSeq())) |> should equal [1; -2]
    
    [<Test>]
    let ``Take 3 elements of seq``() =
        Seq.toList(Seq.take 3 (generateSeq())) |> should equal [1; -2; 3]
    
    [<Test>]
    let ``Take 4 elements of seq``() =
        Seq.toList(Seq.take 4 (generateSeq())) |> should equal [1; -2; 3; -4]
    
    [<Test>]
    let ``Take 5 elements of seq``() =
        Seq.toList(Seq.take 5 (generateSeq())) |> should equal [1; -2; 3; -4; 5]
