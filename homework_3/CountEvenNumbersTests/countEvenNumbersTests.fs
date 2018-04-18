open NUnit.Framework
[<TestFixture>]
module CountEvenNumbersTests =
    open CountEvenNumbers
    
    let testData () =
        [ 
            TestCaseData([] : int list).Returns(0)
            TestCaseData([-2; 1; 2; 3; 5; 6]).Returns(3)
            TestCaseData([1..1000]).Returns(500)
            TestCaseData([-500..500]).Returns(501)
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