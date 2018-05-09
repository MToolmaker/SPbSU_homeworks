module Tests
    open NUnit.Framework
    open FsUnit
    open TaskModule

    [<Test>]
    let ``1``() =
        t() |> should equal true
