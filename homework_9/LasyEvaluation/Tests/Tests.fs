module LazyEvalTests
    open NUnit.Framework
    open FsUnit
    open ILazy
    open LazyFactory
    open System.Threading
    open System

    [<Test>]
    let ``SingleThreadedLazy test`` () =
        let mutable count = 0
        let supplier () =
            count <- count + 1
            4
        let eval = LazyFactory.CreateSingleThreadedLazy(supplier)
        eval.Get() |> ignore
        count |> should equal 1
        eval.Get() |> ignore
        count |> should equal 1
    
    [<Test>]
    let ``MultiThreadedLazy test 1`` () =
        let mutable count = 0
        let supplier () =
            count <- count + 1
            4
        let eval = LazyFactory.CreateMultiThreadedLazy(supplier)
        eval.Get() |> ignore
        count |> should equal 1
        eval.Get() |> ignore
        count |> should equal 1
    
    //С некоторой вероятностью данный тест может пройти и SingleThreadedLazy, т.к. гонка может и не возникнуть.
    [<Test>]
    let ``MultiThreadedLazy test 2`` () =
        for _ in [1..100] do
            let mutable count = 0
            let supplier () =
                count <- count + 1
                1
            let eval = LazyFactory.CreateMultiThreadedLazy(supplier)
            let threads = [for _ in [1..100] -> new Thread(fun () -> eval.Get() |> ignore)]
            List.map (fun (thread : System.Threading.Thread) -> thread.Start()) threads |> ignore
        
            List.map (fun (thread : System.Threading.Thread) -> thread.Join()) threads |> ignore
            count |> should equal 1
    

       
    type RefInt(i : int) =
        member __.I = i
        static member (=) (i : int, ri : RefInt) =
            i = ri.I

        static member (=) (ri : RefInt, i : int) =
            i = ri.I
            
    [<Test>]
    let ``LockFreeLazy test 1`` () =
        let mutable count = 0
        let supplier () =
            let mutable value = "" 
            if count = 0 then
                value <- "1"
            else
                value <- "2"
            count <- count + 1
            value

        let eval = LazyFactory.CreateLockFreeLazy(supplier)
        eval.Get() |> should equal "1"
        count |> should equal 1
        eval.Get() |> should equal "1"
        count |> should equal 1
    
    [<Test>]
    let ``LockFreeLazy test 2`` () =
        let mutable count = 0
        let supplier () =
            let mutable value = "" 
            if count = 0 then
                value <- "1"
            else
                value <- "2"
            count <- count + 1
            value

        let eval = LazyFactory.CreateLockFreeLazy(supplier)
        let threads = [for _ in [1..100] -> new Thread(fun () -> eval.Get() |> should equal "1")]
        List.map (fun (thread : System.Threading.Thread) -> thread.Start()) threads |> ignore
        List.map (fun (thread : System.Threading.Thread) -> thread.Join()) threads |> ignore
