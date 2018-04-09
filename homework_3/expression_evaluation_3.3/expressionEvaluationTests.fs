module expressionEvaluationTests
    open NUnit.Framework
    open FsUnit
    open expressionEvaluation

    [<Test>]
    let ``15 must be 15`` () =
        eval (Value(15)) |> should equal 15
    
    [<Test>]
    let ``(1+9) must be 10`` () =
        eval (Add(Value(1),Value(9))) |> should equal 10

    [<Test>]
    let ``(1-9) must be -8`` () =
        eval (Subtract(Value(1),Value(9))) |> should equal -8

    [<Test>]
    let ``(1+9)*2 must be 20`` () =
        eval (Multiply((Add(Value(1),Value(9))), Value(2))) |> should equal 20

    [<Test>]
    let ``(1+9)/2 must be 5`` () =
        eval (Divide((Add(Value(1),Value(9))), Value(2))) |> should equal 5

    [<Test>]
    let ``(1+9)/3 must fail`` () =
        (eval (Divide((Add(Value(1),Value(9))), Value(3)))) |> should throw typeof<System.Exception>
    [<Test>]
    let ``(1+9)/0 must fail`` () =
        (eval (Divide((Add(Value(1),Value(9))), Value(0)))) |> should (throwWithMessage "Dividing by zero") typeof<System.Exception>
