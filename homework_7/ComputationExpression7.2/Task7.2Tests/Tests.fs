module Tests
    open NUnit.Framework
    open FsUnit
    open Builder

    [<Test>]
    let ``"1"+"2" should be Some(3)``() =
        let maybe = new MaybeBuilder()
        let expectedResult = Some(3)
        let actualResult = maybe{
            let! a = "1"
            let! b = "2"
            return a + b
        }
        actualResult |> should equal expectedResult
    
    [<Test>]
    let ``"a"+"2" should be None``() =
        let maybe = new MaybeBuilder()
        let expectedResult = None
        let actualResult = maybe{
            let! a = "a"
            let! b = "2"
            return a + b
        }
        actualResult |> should equal expectedResult
    
    [<Test>]
    let ``"1"+"b" should be None``() =
        let maybe = new MaybeBuilder()
        let expectedResult = None
        let actualResult = maybe{
            let! a = "1"
            let! b = "b"
            return a + b
        }
        actualResult |> should equal expectedResult
    
    [<Test>]
    let ``"1"+2+"3" should be Some(6)``() =
        let maybe = new MaybeBuilder()
        let expectedResult = Some(6)
        let actualResult = maybe{
            let! a = "1"
            let b = 2
            let! c = "3"
            return a + b + c
        }
        actualResult |> should equal expectedResult
    
    [<Test>]
    let ``"1"+2+"a" should be None``() =
        let maybe = new MaybeBuilder()
        let expectedResult = None
        let actualResult = maybe{
            let! a = "1"
            let b = 2
            let! c = "a"
            return a + b + c
        }
        actualResult |> should equal expectedResult


