module Builder
     //Let's use an active pattern like this c:
    let (|Int|_|) str =
        match System.Int32.TryParse(str) with
        | (true,int) -> Some(int)
        | _ -> None
    
    type MaybeBuilder() =
        member this.Bind(x, f) =
            match x with
            | Int x -> f x
            | _ -> None
        member this.Return(x : int) =
            Some(x)
    
           
