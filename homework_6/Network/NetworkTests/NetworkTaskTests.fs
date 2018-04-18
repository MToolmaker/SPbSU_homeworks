module NetworkTaskTests
    open NetworkTask
    open NUnit.Framework
    open FsUnit

    [<Test>]
    let ``Attempt to add computer with existing id``()=
        let probFun = getInfectionProbability (0.5, 0.2, 0.3)
        let network = new Network("TestNetwork", probFun)

        network.AddComputer 1 OS.Windows true |> ignore
        network.AddComputer 1 OS.Linux false |> should equal false

    [<Test>]
    let ``Attempt to add correct connection``()=
        let probFun = getInfectionProbability (0.5, 0.2, 0.3)
        let network = new Network("TestNetwork", probFun)

        network.AddComputer 1 OS.Windows true |> ignore
        network.AddComputer 2 OS.Linux false |> ignore
        network.Connect 1 2 |> should equal true
    
    [<Test>]
    let ``Attempt to add incorrect connection``()=
        let probFun = getInfectionProbability (0.5, 0.2, 0.3)
        let network = new Network("TestNetwork", probFun)

        network.AddComputer 1 OS.Windows true |> ignore
        network.AddComputer 2 OS.Linux false |> ignore
        network.Connect 1 3 |> should equal false

        
