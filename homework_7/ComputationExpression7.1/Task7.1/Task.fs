module Rounding
    open System

    let (/) a b = if b = 0.0 then None else Some(a/b)

    type MaybeRoundingBuilder(precision : int) =
        do if precision <= 0 then raise <| System.ArgumentException("Precision can't be " + string precision)
        member this.Bind(x : float option, f) =
            match x with
            | None -> x
            | Some a ->
                Math.Round(a,precision) |> f
        
        member this.Return(x : float) = Math.Round(x,precision) |> Some

    let rounding precision = new MaybeRoundingBuilder(precision)
