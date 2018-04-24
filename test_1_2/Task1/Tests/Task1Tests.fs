module Task1Tests
    open NUnit.Framework
    open FsUnit
    open System
    open Task1
    let epsilon = 0.01

    let rec check (list1 : float list) (list2 : float list) =
        match (list1, list2) with
        | ([],[]) -> true
        | _ when abs <| List.head list1 - List.head list2 |> (>) epsilon -> check  (List.tail list1) (List.tail list2)
        | _ -> false

    [<Test>]
    let ``supermap [] must be []``() =
        supermap [] |> should equal []
    
    [<Test>]
    let ``supermap [0.0] must be [0.0; 1.0]``() =
        supermap [] |> should equal []
    
    [<Test>]
    let ``supermap [pi] must be about [0.0; -1.0]``() =
        let sourceList = [Math.PI]
        let expected = [0.0; -1.0]
        check (supermap sourceList) expected |> should equal true
    
    [<Test>]
    let ``supermap [pi/2] must be about [1.0; 0.0]``() =
        let sourceList = [Math.PI/2.0]
        let expected = [1.0; 0.0]
        check (supermap sourceList) expected |> should equal true
    
    [<Test>]
    let ``supermap [2*pi] must be about [0.0; 1.0]``() =
        let sourceList = [2.0 * Math.PI]
        let expected = [0.0; 1.0]
        check (supermap sourceList) expected |> should equal true
    
    [<Test>]
    let ``supermap [pi/2; 0] must be about [1.0; 0.0; 0.0; 1.0]``() =
        let sourceList = [Math.PI/2.0; 0.0]
        let expected = [1.0; 0.0; 0.0; 1.0]
        check (supermap sourceList) expected |> should equal true
    
    [<Test>]
    let ``supermap [pi/4; pi/2] must be about [sqrt(2)/2; sqrt(2)/2; 1.0; 0.0]``() =
        let sourceList = [Math.PI/4.0; Math.PI/2.0]
        let expected = [sqrt(2.0)/2.0; sqrt(2.0)/2.0; 1.0; 0.0]
        check (supermap sourceList) expected |> should equal true
            
        

