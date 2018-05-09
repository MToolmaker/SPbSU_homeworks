module Tests
    open NUnit.Framework
    open FsUnit
    open TaskModule

    [<Test>]
    let ``Adding and contains functions tests``() =
        let hashtable1 = new Hashtable<int, string>(3, fun x -> x)
        hashtable1.Contains 1 "John" |> should equal false
        hashtable1.Add 1 "John"
        hashtable1.Contains 1 "John" |> should equal true
        hashtable1.Contains 2 "Sam" |> should equal false
        hashtable1.Add 2 "Sam"
        hashtable1.Contains 2 "Sam" |> should equal true

        let hashtable2 = new Hashtable<string, string>(3, fun firstName -> firstName.Length)
        hashtable2.Contains "John" "Smith" |> should equal false
        hashtable2.Add "John" "NotSmith"
        hashtable2.Contains "John" "Smith" |> should equal false
        hashtable2.Add "John" "Smith"
        hashtable2.Contains "John" "Smith" |> should equal true

    
    [<Test>]
    let ``Deleting function tests``() =
        let hashtable1 = new Hashtable<int, string>(3, fun x -> x)
        hashtable1.Add 1 "John"
        hashtable1.Add 2 "Sam"
        hashtable1.Contains 1 "John" |> should equal true
        hashtable1.Contains 2 "Sam" |> should equal true

        hashtable1.Delete 1 "John"
        hashtable1.Contains 1 "John" |> should equal false
        hashtable1.Contains 2 "Sam" |> should equal true

        hashtable1.Delete 2 "Sam"
        hashtable1.Contains 1 "John" |> should equal false
        hashtable1.Contains 2 "Sam" |> should equal false

        let hashtable2 = new Hashtable<string, string>(3, fun firstName -> firstName.Length)
        hashtable2.Add "John" "Smith"
        hashtable2.Contains "John" "Smith" |> should equal true
        hashtable2.Delete "John" "NotSmith"
        hashtable2.Contains "John" "Smith" |> should equal true
        hashtable2.Delete "John" "Smith"
        hashtable2.Contains "John" "Smith" |> should equal false

        