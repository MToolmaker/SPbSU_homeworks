module NetworkTask
    open System
    type OS =
        | Windows
        | Linux
        | MacOS

    let getInfectionProbability (windows : float, linux : float, macOs : float) (os : OS) =
        match os with
        | Windows -> windows
        | Linux -> linux
        | MacOS -> macOs

    type Computer(id : int, os : OS, isInfected : bool) =
        let mutable adjacentComputers : Computer list = []

        member val ID = id with get

        member val OS = os with get

        member this.AdjacentComputers
            with get() = adjacentComputers
    
        member val Infected = isInfected with get, set

        member this.IsConnected(id) =
            List.exists (fun (comp : Computer) -> comp.ID = id) adjacentComputers

        member this.Connect (a : Computer) =
            if this.ID <> a.ID && not (this.IsConnected a.ID) then
                adjacentComputers <- a :: adjacentComputers
                a.Connect(this)


    
    type Network(name : string, getInfectionProbability : OS -> float) =
        let mutable nodes : Computer list = []
        let mutable turn = 0
        let rand = new Random()

        let showIds list = List.map (fun id -> printfn "ID: %d" id) list |> ignore
        let infectConnectedComputers list =
            list 
            |> List.filter (fun (comp : Computer) -> rand.NextDouble() < getInfectionProbability(comp.OS))
            |> List.map (fun (comp : Computer) -> comp.Infected <- true)
            |> ignore
        
        member val Name = name with get

        member __.Turn with get() = turn

        member this.AddComputer id os isInfected =
            if List.exists (fun (comp : Computer) -> comp.ID = id) nodes then
                false
            else
                nodes <- (new Computer(id, os, isInfected)) :: nodes
                true
    
        member __.Connect id1 id2 =
            try
                let comp1 = List.find (fun (comp : Computer) -> comp.ID = id1) nodes
                let comp2 = List.find (fun (comp : Computer) -> comp.ID = id2) nodes
                comp1.Connect(comp2)
                true
            with
                | :? System.Collections.Generic.KeyNotFoundException -> false

        member __.IDs with get() = nodes |> List.map (fun (comp : Computer) -> comp.ID)
        
        member this.ShowAllComputers() =
            this.IDs |> showIds
       
        member __.CleanIDs with get() = nodes |> List.filter (fun (comp : Computer) -> not comp.Infected) |> List.map (fun (comp : Computer) -> comp.ID)
        
        member this.ShowCleanComputers() =
            this.CleanIDs |> showIds
        
        member __.InfectedIDs with get() = nodes |> List.filter (fun (comp : Computer) -> comp.Infected) |> List.map (fun (comp : Computer) -> comp.ID)
       
        member this.ShowInfectedComputers() =
            this.InfectedIDs |> showIds
        
        member __.TakeTurn() =
            nodes
            |> List.filter (fun (comp : Computer) -> comp.Infected)
            |> List.map (fun (infected : Computer) -> infectConnectedComputers infected.AdjacentComputers)
            |> ignore
            turn <- turn + 1

    let networkInteractive (network : Network) =
        printfn "Welcome. It's network interactive."
    
        let rec handleCommand () =
            printfn "Enter command: (h - help)"
            let input = Console.ReadKey().KeyChar
            printfn "\n"
            match input with
            | 'h' -> printfn "Take turn:q"
                     printfn "Show turn:a"
                     printfn "Show all computers: w"
                     printfn "Show clean computers: e"
                     printfn "Show infected computers: r"
                     printfn "Exit: t"
                     handleCommand()
            | 'a' -> printfn "Current turn is %d" network.Turn
                     handleCommand()
            | 'q' -> network.TakeTurn()
                     printfn "Now, turn is %d" network.Turn
                     handleCommand()
            | 'w' -> network.ShowAllComputers()
                     printfn "All computers in network:"
                     handleCommand()
            | 'e' -> network.ShowCleanComputers()
                     printfn "Clean computers in network:"
                     handleCommand()
            | 'r' -> network.ShowInfectedComputers()
                     printfn "Infected computers in network:"
                     handleCommand()
            | 't' -> printfn "Exiting.."
            | _ ->   printfn "Incorrect command."
                     handleCommand()
        handleCommand ()

