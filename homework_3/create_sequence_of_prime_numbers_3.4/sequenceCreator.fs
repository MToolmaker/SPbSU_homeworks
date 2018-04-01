module sequenceCreator
    let createSequence() = 
        let isPrimeNumber n =
             [2..n/2] |> Seq.exists(fun x -> n % x = 0) |> not 
        Seq.initInfinite(fun x -> x + 2) |> Seq.filter isPrimeNumber
      
