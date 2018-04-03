module ``findSinAverage tests`` =
    open NUnit.Framework
    open FsUnit
    open sinAverage
    let epsilon = 0.01

    [<Test>]
    let ``Sin Average [-1.0 ; 1.0; 3.0 * 1.54] must be about 1`` () =
        abs ((findSinAverage [-1.0 ; 1.0; 3.0 * 1.54]) - 1) < epsilon |> should equal true

    [<Test>]
    let ``Sin Average [-1.0 ; 0.0; 1.0] must be about 0`` () =
        abs ((findSinAverage [-1.0 ; 0.0; 1.0])) < epsilon |> should equal true

    [<Test>]
    let ``Sin Average [] must be about 0`` () =
        abs ((findSinAverage [])) < epsilon |> should equal true
