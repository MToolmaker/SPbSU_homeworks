module PointFreeTaskTests
    open NUnit.Framework
    open FsCheck
    open PointFreeTask

    [<Test>]
    let ``Equality: func'0 and func'1`` () =
        Check.Quick (fun x l -> func'0 x l = func'1 x l)
    
    [<Test>]
    let ``Equality: func'1 and func'2`` () =
        Check.Quick (fun x l -> (func'1 x l = func'2 x l))
    
    [<Test>]
    let ``Equality: func'2 and func'3`` () =
        Check.Quick (fun x l -> (func'2 x l = func'3 x l))
    
    [<Test>]
    let ``Equality: func'3 and func'0`` () =
        Check.Quick (fun x l -> (func'3 x l = func'0 x l))
