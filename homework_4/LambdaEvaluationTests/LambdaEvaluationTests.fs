module LambdaEvaluationTests'
    open NUnit.Framework
    open FsUnit
    open LambdaEvaluation

    [<Test>]
    let ``x ->> x`` () =
        betaReduce (Variable("x"))
        |> should equal (Variable("x"))

    [<Test>]
    let ``x x ->> x x`` () =
        betaReduce (App(Variable("x"), Variable("x")))
        |> should equal (App(Variable("x"), Variable("x")))

    [<Test>]
    let ``x (λx.x) ->> x (λx.x)`` () =
        betaReduce (App(Variable("x"), Abstr("x", Variable("x"))))
        |> should equal (App(Variable("x"), Abstr("x", Variable("x"))))

    [<Test>]
    let ``λz.(λx.x z) (m v) ->> λx.x m v`` () =
        betaReduce (App(Abstr("z",Abstr("x",App(Variable("x"), Variable("z")))), App(Variable("m"), Variable("v"))))
        |> should equal (Abstr("x",App(Variable("x"), App(Variable("m"), Variable("v")))))

    [<Test>]
    let ``(λx.x) ->> (λx.x)`` () = 
        betaReduce (Abstr("x", Variable("x")))
        |> should equal (Abstr("x", Variable("x")))
    
    [<Test>]
    let ``a b c ->> a b c`` () =
        betaReduce (App(App(Variable("a"),Variable("b")), Variable("c")))
        |> should equal (App(App(Variable("a"),Variable("b")), Variable("c")))
    
    [<Test>]
    let ``(((λa.(λb.a)) (λx.x)) ((λs.s s) (λs.s s))) ->> (λx.x)`` () =
        betaReduce (App(App(Abstr("a", Abstr("b", Variable("a"))), Abstr("x", Variable("x"))), App(Abstr("s", App(Variable("s"), Variable("s"))), Abstr("s", App(Variable("s"), Variable("s"))))))
        |> should equal (Abstr("x", Variable("x")))
    
    //Тесты на переименование связанных переменных
    [<Test>]
    let ``λy.λx.(x y) x ->> λx'.(x' x)`` () =
        betaReduce (App(Abstr("y",Abstr("x",App(Variable("x"), Variable("y")))),Variable("x")))
        |> should equal (Abstr("x'", App(Variable("x'"),Variable("x"))))
    
    [<Test>]
    let ``λx.λy.λz(z y x) z z'->> λz''.(z'' z' z)`` () =
        betaReduce (App(App(Abstr("x",Abstr("y",Abstr("z",App(App(Variable("z"),Variable("y")),Variable("x"))))),Variable("z")),Variable("z'")))
        |> should equal (Abstr("z''", App(App(Variable("z''"),Variable("z'")),Variable("z"))))