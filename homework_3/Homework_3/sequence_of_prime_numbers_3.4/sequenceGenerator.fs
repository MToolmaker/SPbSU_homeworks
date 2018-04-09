module sequenceGenerator
    let createSequence() = 
        let isPrimeNumber n =
            let k = float n |> sqrt |> int
            [2..k] |> Seq.exists(fun x -> n % x = 0) |> not 
        Seq.initInfinite(fun x -> x + 2) |> Seq.filter isPrimeNumber
