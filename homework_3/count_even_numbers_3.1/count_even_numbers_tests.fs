module ``foldEvenNumbers tests`` =
    open NUnit.Framework
    open FsUnit
    open count_even_numbers
    [<Test>]
    let ``countEvenNumbers1 [-2; 1; 2; 3; 5; 6] must return 3`` () =
        foldEvenNumbers [-2; 1; 2; 3; 5; 6] |> should equal 3

    [<Test>]
    let ``countEvenNumbers1 [] must return 0`` () =
        foldEvenNumbers [] |> should equal 0
    
    [<Test>]
    let ``countEvenNumbers1 [1..1000] must return 500`` () =
        foldEvenNumbers [1..1000] |> should equal 500
    
    [<Test>]
    let ``countEvenNumbers1 [-500..500] must return 501`` () =
        foldEvenNumbers [-500..500] |> should equal 501

module ``filterCountEvenNumbers tests`` =
    open NUnit.Framework
    open FsUnit
    open count_even_numbers
    [<Test>]
    let ``countEvenNumbers2 [-2; 1; 2; 3; 5; 6] must return 3`` () =
        filterCountEvenNumbers [-2; 1; 2; 3; 5; 6] |> should equal 3

    [<Test>]
    let ``countEvenNumbers2 [] must return 0`` () =
        filterCountEvenNumbers [] |> should equal 0
    
    [<Test>]
    let ``countEvenNumbers2 [1..1000] must return 500`` () =
        filterCountEvenNumbers [1..1000] |> should equal 500
    
    [<Test>]
    let ``countEvenNumbers2 [-500..500] must return 501`` () =
        filterCountEvenNumbers [-500..500] |> should equal 501

module ``mapCountEvenNumbers tests`` =
    open NUnit.Framework
    open FsUnit
    open count_even_numbers

    [<Test>]
    let ``countEvenNumbers3 [-2; 1; 2; 3; 5; 6] must return 3`` () =
        mapCountEvenNumbers [-2; 1; 2; 3; 5; 6] |> should equal 3

    [<Test>]
    let ``countEvenNumbers3 [] must return 0`` () =
        mapCountEvenNumbers [] |> should equal 0
    
    [<Test>]
    let ``countEvenNumbers3 [1..1000] must return 500`` () =
        mapCountEvenNumbers [1..1000] |> should equal 500
    
    [<Test>]
    let ``countEvenNumbers3 [-500..500] must return 501`` () =
        mapCountEvenNumbers [-500..500] |> should equal 501
