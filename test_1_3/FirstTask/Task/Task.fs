module TaskModule
    let generateSeq length =
        let seq = Seq.initInfinite (fun index -> if index % 2 = 0 then 1 else -1)
        Seq.initInfinite (fun index -> (index + 1) * (Seq.item index seq))
