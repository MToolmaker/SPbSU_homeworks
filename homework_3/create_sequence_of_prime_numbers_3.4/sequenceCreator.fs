module sequenceCreator
    let createSequence() = 
        let isPrimeNumber n =
             let k = float n |> sqrt |> int
             [2..k+1] |> Seq.exists(fun x -> n % x = 0) |> not 
        Seq.initInfinite(fun x -> x + 2) |> Seq.filter isPrimeNumber
      
