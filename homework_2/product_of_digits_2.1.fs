//This function receives a positive integer
let productOfDigits n =
    if n > 0 then
        let rec productOfDigits n acc = 
            if n / 10 = 0 then
                acc * (n % 10)
            else
                productOfDigits (n / 10) (acc * (n % 10))
        productOfDigits n 1
    else
        -1

let first = productOfDigits 1
let second = productOfDigits 12
let third = productOfDigits 325
