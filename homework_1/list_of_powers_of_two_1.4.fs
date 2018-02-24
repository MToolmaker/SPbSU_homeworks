let getPowersOfTwo n m =
    if n >=0 && m >= 0 then
        let powerOfTwo n = 
            let rec power i acc =
                if i = n + 1 then
                    acc
                else
                    power (i + 1) (acc * 2)
            power 1 1

        let lastNumber = powerOfTwo (n + m)

        let rec getPowersOfTwo list acc i =
            if i = 0 then
                list
            else
                getPowersOfTwo ((acc / 2) :: list) (acc / 2) (i - 1)
        getPowersOfTwo [lastNumber] lastNumber m
    else
        []

let powers_1 = getPowersOfTwo 5 7
let powers_2 = getPowersOfTwo 4 3
let powers_3 = getPowersOfTwo 2 5
let powers_4 = getPowersOfTwo 1 4
let powers_5 = getPowersOfTwo 0 0
let powers = getPowersOfTwo -1 -1
