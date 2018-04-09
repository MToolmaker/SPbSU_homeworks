module MapToTreeTests =
    open NUnit.Framework
    open FsUnit
    open MapToTree

    [<Test>]
    let ``map Tree.Empty must be Tree.Empty`` () =
        map Tree.Empty (fun x -> x) |> should equal Tree.Empty

    [<Test>]
    let ``map (Tree(3, Tree.Empty, Tree.Empty)) (fun x -> x*x) must be Tree.Empty`` () =
        map (Tree(3, Tree.Empty, Tree.Empty)) (fun x -> x*x) |> should equal (Tree(9, Tree.Empty, Tree.Empty))

    [<Test>]
    let ``map (Tree(5, Tree(7, Tree.Empty, Tree(2, Tree.Empty, Tree.Empty)), Tree.Empty)) (fun x -> 2*x) must be Tree.Empty`` () =
        map (Tree(5, Tree(7, Tree.Empty, Tree(2, Tree.Empty, Tree.Empty)), Tree.Empty)) (fun x -> 2*x) |>
        should equal (Tree(10, Tree(14, Tree.Empty, Tree(4, Tree.Empty, Tree.Empty)), Tree.Empty))