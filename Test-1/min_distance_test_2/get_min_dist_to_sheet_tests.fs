module ``getMinDistToSheet tests`` =
    open NUnit.Framework
    open FsUnit
    open minDistance

    [<Test>]
    let ``getMinDistToSheet Empty must be None`` () =
        getMinDistToSheet Empty |> should equal None

    [<Test>]
    let ``getMinDistToSheet (Tree("hah", Tree("hah", Empty, Empty), Empty)) must be Some(1)`` () =
        getMinDistToSheet (Tree("hah", Tree("hah", Empty, Empty), Empty)) |> should equal Some(1)

    [<Test>]
    let ``getMinDistToSheet (Tree("hah", Empty, Tree("hah", Empty, Empty))) must be Some(1)`` () =
        getMinDistToSheet (Tree("hah", Empty, Tree("hah", Empty, Empty))) |> should equal Some(1)

    [<Test>]
    let ``getMinDistToSheet (Tree("hah", Tree("hah", Empty, Empty), Tree("hah", Empty, Empty))) must be Some(2)`` () =
        getMinDistToSheet (Tree("hey", Tree("hah", Empty, Empty), Tree("hoh", Empty, Empty))) |> should equal Some(2)
