open NUnit.Framework
[<TestFixture>]
module ``countEvenNumbers tests`` =
    open countEvenNumbers
    let emptyList: int list = []
    let testData () =
        [ 
            TestCaseData(emptyList).Returns(None)
            TestCaseData([-2; 1; 2; 3; 5; 6]).Returns(Some(3))
            TestCaseData([1..1000]).Returns(Some(500))
            TestCaseData([-500..500]).Returns(Some(501))
        ]

    [<TestCaseSource("testData")>]
    let foldCountEvenNumbersTests list = 
        foldCountEvenNumbers list

    [<TestCaseSource("testData")>]
    let filterCountEvenNumbersTests list = 
        filterCountEvenNumbers list

    [<TestCaseSource("testData")>]
    let mapCountEvenNumbersTests list = 
        mapCountEvenNumbers list
