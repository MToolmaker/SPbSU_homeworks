let factorial n =
    //Incorrect argument
    if n < 1 then
        -1
    else
    let rec factorial n acc =
        if n > 1 then
            factorial (n - 1) (acc * n)
        else
            acc
    factorial n 1

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
