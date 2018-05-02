module Tests
    open NUnit.Framework
    open FsUnit
    open Rounding

    [<Test>]
    let ``(12.0 / 2.0) / 3.0 should be Some(2.0)``() =
        let testValue =
            rounding 2 {
                let! a = 12.0 / 2.0
                let! b = a / 3.0
                return b
            }
        testValue |> should equal <| Some(2.0)
    
    [<Test>]
    let ``(12.0 / 0.0) / 3.0 should be None``() =
        let testValue =
            rounding 2 {
                let! a = 12.0 / 0.0
                let! b = a / 3.0
                return b
            }
        testValue |> should equal <| None
    
    [<Test>]
    let ``(1.0 / 3.0) with precision 1 digit after decimal point should be Some(0.3)``() =
        let testValue =
            rounding 1 {
                let! a = 1.0 / 3.0
                return a
            }
        testValue |> should equal <| Some(0.3)

    [<Test>]
    let ``(1.0 / 3.0) with precision 2 digits after decimal point should be Some(0.33)``() =
        let testValue =
            rounding 2 {
                let! a = 1.0 / 3.0
                return a
            }
        testValue |> should equal <| Some(0.33)
    
    [<Test>]
    let ``(10.0 / 8.0) with precision 1 digit after decimal point should be Some(1.2)``() =
        let testValue =
            rounding 1 {
                let! a = 10.0 / 8.0
                return a
            }
        testValue |> should equal <| Some(1.2)

    [<Test>]
    let ``(10.0 / 8.0) with precision 2 digits after decimal point should be Some(1.25)``() =
        let testValue =
            rounding 2 {
                let! a = 10.0 / 8.0
                return a
            }
        testValue |> should equal <| Some(1.25)