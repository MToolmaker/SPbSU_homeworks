let fibonacci n = 
    if n <= 2 then
        1
    else
    let rec fibonacci x y i = 
        if i < n then
            fibonacci y (x + y) (i + 1)
        else
            y
    fib 1 1 2
