//This function receives a positive integer
let productOfDigits n =
    let rec productOfDigits n acc = 
        if n / 10 = 0 then
            acc * (n % 10)
        else
            productOfDigits (n / 10) (acc * (n % 10))
    productOfDigits (abs n) 1

let first = productOfDigits 1
let second = productOfDigits 123
let third = productOfDigits 325
let fourth = productOfDigits 0
