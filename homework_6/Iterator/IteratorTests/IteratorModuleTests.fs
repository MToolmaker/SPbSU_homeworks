module IteratorModuleTests
    open NUnit.Framework
    open FsUnit
    open IteratorModule
    type TestType =
        {id : int
         name : string}
    
    module IsBinarySearchTreeFunctionTests =

        [<Test>]
        let ``Checking binary search trees``() =
            let testTree'1 = Empty
            isBinarySearchTree testTree'1 |> should equal true

            let testTree'2 = Node((1, 1), Empty, Empty)
            isBinarySearchTree testTree'2 |> should equal true

            let leftTestTree = Node((0, 0), Empty, Empty)
            let rightTestTree = Node((2, 2), Empty, Empty)

            let testTree'3 = Node((1, 1), leftTestTree, Empty)
            isBinarySearchTree testTree'3 |> should equal true

            let testTree'4 = Node((1, 1), Empty, rightTestTree)
            isBinarySearchTree testTree'4 |> should equal true

            let testTree'5 = Node((1, 1), leftTestTree, rightTestTree)
            isBinarySearchTree testTree'5 |> should equal true
        
        [<Test>]
        let ``Checking non-search binary trees``() =

            let leftTestTree = Node((0, 0), Empty, Empty)
            let rightTestTree = Node((2, 2), Empty, Empty)

            let testTree'1 = Node((1, 1), Empty, leftTestTree)
            isBinarySearchTree testTree'1 |> should equal false

            let testTree'2 = Node((1, 1), rightTestTree, Empty)
            isBinarySearchTree testTree'2 |> should equal false

            let testTree'3 = Node((10, 1), leftTestTree, rightTestTree)
            isBinarySearchTree testTree'3 |> should equal false

    module FindFunctionTests =

        [<Test>]
        let ``Searching by int key in binary search tree``() =
            let testRecord'1 = {id = 1; name = "John"}
            let testRecord'2 = {id = 2; name = "David"}
            let testRecord'3 = {id = 3; name = "Sam"}

            let left = Node((testRecord'1.id, testRecord'1.name), Empty, Empty)
            let right = Node((testRecord'3.id, testRecord'3.name), Empty, Empty)

            let testTree = Node((testRecord'2.id, testRecord'2.name), left, right)
        
            find 0 testTree |> should equal None
            find 1 testTree |> should equal <| Some(testRecord'1.name)
            find 2 testTree |> should equal <| Some(testRecord'2.name)
            find 3 testTree |> should equal <| Some(testRecord'3.name)
            find 4 testTree |> should equal None
        
        [<Test>]
        let ``Searching by string key in binary search tree``() =
            let testRecord'1 = {id = 1; name = "John"}
            let testRecord'2 = {id = 2; name = "David"}
            let testRecord'3 = {id = 3; name = "Sam"}

            let left = Node((testRecord'2.name, testRecord'2.id), Empty, Empty)
            let right = Node((testRecord'3.name, testRecord'3.id), Empty, Empty)

            let testTree = Node((testRecord'1.name, testRecord'1.id), left, right)

            find "SomeName1" testTree |> should equal None
            find "John" testTree |> should equal <| Some(testRecord'1.id)
            find "David" testTree |> should equal <| Some(testRecord'2.id)
            find "Sam" testTree |> should equal <| Some(testRecord'3.id)
            find "SomeName2" testTree |> should equal None
    
    module InsertFunctionTests =

        [<Test>]
        let ``Insert into an empty tree``() =
            let testTree = Empty
            insert 1 1 testTree |> should equal <| Node((1,1), Empty, Empty)

        [<Test>]
        let ``Insert value (with highest key than tree has) into a binary search tree``() =
            let testTree = Node((1,1), Empty, Empty)
            insert 2 2 testTree |> should equal <| Node((1,1), Empty, Node((2,2), Empty, Empty))

        [<Test>]
        let ``Insert value (with least key than tree has) into a binary search tree``() =
            let testTree = Node((1,1), Empty, Empty)
            insert 0 0 testTree |> should equal <| Node((1,1), Node((0,0), Empty, Empty), Empty)
        
        [<Test>]
        let ``Insert value advanced test 1``() =
             let leftTree = Node((1,1), Empty, Empty)
             let testTree = Node((3,3), leftTree, Empty)
             insert 2 2 testTree |> should equal <| Node((3,3), Node((1,1), Empty, Node((2,2), Empty, Empty)), Empty)
        
        [<Test>]
        let ``Insert value advanced test 2``() =
             let leftTree = Node((1,1), Empty, Empty)
             let testTree = Node((2,2), leftTree, Empty)
             insert 0 0 testTree |> should equal <| Node((2,2), Node((1,1), Node((0,0), Empty, Empty), Empty), Empty)
        
        [<Test>]
        let ``Insert value advanced test 3``() =
             let rightTree = Node((4,4), Empty, Empty)
             let testTree = Node((2,2), Empty, rightTree)
             insert 3 3 testTree |> should equal <| Node((2,2), Empty, Node((4,4), Node((3,3), Empty, Empty), Empty))

        [<Test>]
        let ``Insert value advanced test 4``() =
             let rightTree = Node((2,2), Empty, Empty)
             let testTree = Node((1,1), Empty, rightTree)
             insert 3 3 testTree |> should equal <| Node((1,1), Empty, Node((2,2), Empty, Node((3,3), Empty, Empty)))

    module DeleteFunctionTests =

        [<Test>]
        let ``Delete from empty tree``() =
            let testTree : Tree<int, 'value> = Empty
            delete 0 testTree |> should equal testTree
            
        [<Test>]
        let ``Delete from binary search tree test 1``() =
            let leftTestTree = Node((0, 0), Empty, Empty)
            let rightTestTree = Node((2, 2), Empty, Empty)
            let testTree = Node((1, 1), leftTestTree, rightTestTree)
            delete 0 testTree |> should equal <| Node((1, 1), Empty, rightTestTree)
            delete 2 testTree |> should equal <| Node((1, 1), leftTestTree, Empty)
            testTree |> delete 0 |> delete 2 |> should equal <| Node((1, 1), Empty, Empty)
        

        [<Test>]
        let ``Delete from binary search tree test 2``() =
            let leftTestTree = Node((0, 0), Empty, Empty)
            let rightTestTree = Node((2, 2), Empty, Empty)
            let testTree = Node((1, 1), leftTestTree, rightTestTree)
            delete 0 testTree |> should equal <| Node((1, 1), Empty, rightTestTree)
            delete 2 testTree |> should equal <| Node((1, 1), leftTestTree, Empty)
            testTree |> delete 0 |> delete 2 |> should equal <| Node((1, 1), Empty, Empty)
            delete 1 testTree |> should equal <| Node((0, 0), Empty, Node((2, 2), Empty, Empty))
        
        [<Test>]
        let ``Delete from binary search tree test 3``() =
            let testTree = Node((0,0), Empty, Node((1,1), Empty, Node((2,2), Empty, Empty)))
            delete 0 testTree |> should equal <| Node((1, 1), Empty, Node((2, 2), Empty, Empty))
            delete 1 testTree |> should equal <| Node((0, 0), Empty, Node((2, 2), Empty, Empty))
            delete 2 testTree |> should equal <| Node((0, 0), Empty, Node((1, 1), Empty, Empty))
        
        [<Test>]
        let ``Delete from binary search tree test 4``() =
            let rightTestSubTree = Node((1, 1), Empty, Empty)
            let leftTestTree = Node((0, 0), Empty, rightTestSubTree)
            let rightTestTree = Node((3, 3), Empty, Empty)
            let testTree = Node((2, 2), leftTestTree, rightTestTree)
            delete 0 testTree |> should equal <| Node((2, 2), Node((1, 1), Empty, Empty), Node((3, 3), Empty, Empty))
            delete 1 testTree |> should equal <| Node((2, 2), Node((0, 0), Empty, Empty), Node((3, 3), Empty, Empty))
            delete 2 testTree |> should equal <| Node((1, 1), Node((0, 0), Empty, Empty), Node((3, 3), Empty, Empty))

    module IteratorInterfaceTests =
        [<Test>]
        let ``Iterate through empty tree``() =
            let mutable flag = true
            for node in Empty do
                flag <- false
            flag |> should equal true
        
        [<Test>]
        let ``Iterate through one-element tree``() =
            let expectedValue = Some(1)
            let mutable actualValue = None
            let testTree = Node((1,1), Empty, Empty)
            for node in testTree do
                actualValue <- Some(node)
            actualValue |> should equal expectedValue
        
        [<Test>]
        let ``Iterate through three-element binary search tree``() =
            let expectedListOfValues = [1; 2; 3]
            let mutable actualRevertedListOfValues = []
            let leftTree = Node((1,1), Empty, Empty)
            let rightTree = Node((3,3), Empty, Empty)
            let testTree = Node((2,2), leftTree, rightTree)
            for node in testTree do
                actualRevertedListOfValues <- node :: actualRevertedListOfValues
            let actualListOfValues = List.rev actualRevertedListOfValues
            actualListOfValues |> should equal expectedListOfValues
        
        [<Test>]
        let ``Iterate through degenerate binary search tree``() =
            let expectedListOfValues = [1; 2; 3]
            let mutable actualRevertedListOfValues = []
            let testTree'1 = Node((3,3), Empty, Empty)
            let testTree'2 = Node((2,2), Empty, testTree'1)
            let testTree'3 = Node((1,1), Empty, testTree'2)
            for node in testTree'3 do
                actualRevertedListOfValues <- node :: actualRevertedListOfValues
            let actualListOfValues = List.rev actualRevertedListOfValues
            actualListOfValues |> should equal expectedListOfValues
        
        [<Test>]
        let ``Iterate through binary search tree. Advanced test.``() =
            let expectedListOfValues = [0; 1; 2; 3]
            let mutable actualRevertedListOfValues = []
            let rightTestSubTree = Node((1, 1), Empty, Empty)
            let leftTestTree = Node((0, 0), Empty, rightTestSubTree)
            let rightTestTree = Node((3, 3), Empty, Empty)
            let testTree = Node((2, 2), leftTestTree, rightTestTree)
            for node in testTree do
                actualRevertedListOfValues <- node :: actualRevertedListOfValues
            let actualListOfValues = List.rev actualRevertedListOfValues
            actualListOfValues |> should equal expectedListOfValues
        
