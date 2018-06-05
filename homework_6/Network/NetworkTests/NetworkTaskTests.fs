module NetworkTaskTests
    open NetworkTask
    open NUnit.Framework
    open FsUnit

    [<Test>]
    let ``Attempt to add computer with existing id``()=
        let probFun = getInfectionProbability (0.5, 0.2, 0.3)
        let network = new Network("TestNetwork", probFun)

        network.AddComputer 1 OS.Windows true |> should equal true
        network.AddComputer 1 OS.Linux false |> should equal false

    [<Test>]
    let ``Attempt to add correct connection``()=
        let probFun = getInfectionProbability (0.5, 0.2, 0.3)
        let network = new Network("TestNetwork", probFun)

        network.AddComputer 1 OS.Windows true |> should equal true
        network.AddComputer 2 OS.Linux false |> should equal true
        network.Connect 1 2 |> should equal true
    
    [<Test>]
    let ``Attempt to add incorrect connection``()=
        let probFun = getInfectionProbability (0.5, 0.2, 0.3)
        let network = new Network("TestNetwork", probFun)

        network.AddComputer 1 OS.Windows true |> should equal true
        network.AddComputer 2 OS.Linux false |> should equal true
        network.Connect 1 3 |> should equal false
    
    [<Test>]
    let ``Adjacent computers to infected must be infected at every turn. 100% probability of infection for all OS's.``() =
        let probFun = getInfectionProbability (1.0, 1.0, 1.0)
        let network = new Network("TestNetwork", probFun)

        network.AddComputer 1 OS.Windows true |> should equal true
        network.AddComputer 2 OS.Linux false |> should equal true
        network.AddComputer 3 OS.MacOS false |> should equal true
        network.AddComputer 4 OS.Windows false |> should equal true
        network.AddComputer 5 OS.Linux false |> should equal true

        network.Connect 1 2 |> should equal true
        network.Connect 1 3 |> should equal true
        network.Connect 2 4 |> should equal true
        network.Connect 3 4 |> should equal true
        network.Connect 4 5 |> should equal true

        let infected'0 : Set<int> = set network.InfectedIDs
        let expectedInfected'0 : Set<int> = set [1]
        infected'0 |> should equal expectedInfected'0

        network.TakeTurn()
        let infected'1 : Set<int> = set network.InfectedIDs
        let expectedInfected'1 = expectedInfected'0.Add(2).Add(3)

        infected'1 |> should equal expectedInfected'1

        network.TakeTurn()
        let infected'2 : Set<int> = set network.InfectedIDs
        let expectedInfected'2 = expectedInfected'1.Add(4)
        infected'2 |> should equal expectedInfected'2

        network.TakeTurn()
        let infected'3 : Set<int> = set network.InfectedIDs
        let expectedInfected'3 = expectedInfected'2.Add(5)
        infected'3 |> should equal expectedInfected'3

    [<Test>]
    let ``Adjacent computers to infected must be clean at every turn. 0% probability of infection for all OS's.``() =
        let probFun = getInfectionProbability (0.0, 0.0, 0.0)
        let network = new Network("TestNetwork", probFun)

        network.AddComputer 1 OS.Windows true |> should equal true
        network.AddComputer 2 OS.Linux false |> should equal true
        network.AddComputer 3 OS.MacOS false |> should equal true
        network.AddComputer 4 OS.Windows false |> should equal true
        network.AddComputer 5 OS.Linux true |> should equal true

        network.Connect 1 2 |> should equal true
        network.Connect 1 3 |> should equal true
        network.Connect 2 4 |> should equal true
        network.Connect 3 4 |> should equal true
        network.Connect 4 5 |> should equal true 

        for i in 1..100 do
            set network.InfectedIDs |> should equal <| set [1; 5]
            network.TakeTurn()

    [<Test>]
    let ``Linux and MacOS computers must be clean at every turn. Windows 100%, Linux 0%, MacOs 0%.``() =
        let probFun = getInfectionProbability (1.0, 0.0, 0.0)
        let network = new Network("TestNetwork", probFun)
        //Let's suggest infected computer can be added to a network with OS which has 0% probability of being infected.
        network.AddComputer 1 OS.Windows false |> should equal true
        network.AddComputer 2 OS.Linux true |> should equal true
        network.AddComputer 3 OS.MacOS false |> should equal true
        network.AddComputer 4 OS.Linux false |> should equal true
        //This computer will always be clean because there's only one path to it from computer 2 which goes through computer 4 which cannot be infected
        network.AddComputer 5 OS.Windows false |> should equal true

        network.Connect 1 2 |> should equal true
        network.Connect 1 3 |> should equal true
        network.Connect 2 4 |> should equal true
        network.Connect 3 4 |> should equal true
        network.Connect 4 5 |> should equal true 

        let infected'0 : Set<int> = set network.InfectedIDs
        let expectedInfected'0 : Set<int> = set [2]
        infected'0 |> should equal expectedInfected'0

        network.TakeTurn()
        let infected'1 : Set<int> = set network.InfectedIDs
        let expectedInfected'1 = expectedInfected'0.Add(1)

        infected'1 |> should equal expectedInfected'1

        for i in 1..10 do
            set network.InfectedIDs |> should equal expectedInfected'1
            network.TakeTurn()