module checkCorrectnessTests
    open NUnit.Framework
    open FsUnit
    open checkCorrectness

    [<Test>]
    let ``"" must be correct`` () =
        braceChecker "" |> should equal true
    
    [<Test>]
    let ``"()" must be correct`` () =
        braceChecker "()" |> should equal true
    
    [<Test>]
    let ``"[]" must be correct`` () =
        braceChecker "[]" |> should equal true

    [<Test>]
    let ``"{}" must be correct`` () =
        braceChecker "{}" |> should equal true

    [<Test>]
    let ``"[)" must be incorrect`` () =
        braceChecker "[)" |> should equal false

    [<Test>]
    let ``"[]]" must be incorrect`` () =
        braceChecker "[]]" |> should equal false
    
    [<Test>]
    let ``"[[]" must be incorrect`` () =
        braceChecker "[[]" |> should equal false
    
    [<Test>]
    let ``"{}}" must be incorrect`` () =
        braceChecker "{}}" |> should equal false
    
    [<Test>]
    let ``"{{}" must be incorrect`` () =
        braceChecker "{{}" |> should equal false
    
    [<Test>]
    let ``"(())" must be correct`` () =
        braceChecker "(())" |> should equal true
    
    [<Test>]
    let ``"[[]]" must be correct`` () =
        braceChecker "[[]]" |> should equal true

    [<Test>]
    let ``"{{}}" must be correct`` () =
        braceChecker "{{}}" |> should equal true

    [<Test>]
    let ``"aaa(())" must be correct`` () =
        braceChecker "aaa(())" |> should equal true
    
    [<Test>]
    let ``"aaa[[]]" must be correct`` () =
        braceChecker "aaa[[]]" |> should equal true

    [<Test>]
    let ``"aaa{{}}" must be correct`` () =
        braceChecker "aaa{{}}" |> should equal true
    
    [<Test>]
    let ``"{(})" must be incorrect`` () =
        braceChecker "{(})" |> should equal false
    
    [<Test>]
    let ``"a{a(a}a)a" must be incorrect`` () =
        braceChecker "a{a(a}a)a" |> should equal false
    
    [<Test>]
    let ``"a{a{a}a}a" must be correct`` () =
        braceChecker "a{a{a}a}a" |> should equal true

    [<Test>]
    let ``"(a{a{a}a}a)" must be correct`` () =
        braceChecker "(a{a{a}a}a)" |> should equal true
    
    [<Test>]
    let ``"((a{a{a}a}a)" must be incorrect`` () =
        braceChecker "((a{a{a}a}a)" |> should equal false
    
    [<Test>]
    let ``"{{{}}}{((a[{a{a}a}]a)}" must be incorrect`` () =
        braceChecker "{{{}}}{((a[{a{a}a}]a)" |> should equal false

    [<Test>]
    let ``"{}{}{}{()}" must be correct`` () =
        braceChecker "{(a)a(a)a(a)a(a)a(a){sdf}{[a]()fsdf}{ss}{}{a[][]}}" |> should equal true
    