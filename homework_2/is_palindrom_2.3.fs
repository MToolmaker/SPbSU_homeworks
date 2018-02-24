let isPalindrom (text: string) =
    if text = "" then
        true
    else
    let length = text.Length
    let middleOfText = text.Length / 2
    let rec isPalindrom i =
        if i = middleOfText + 1 then
            true
        else if text.[i] <> text.[length - i - 1] then
            false
        else
            isPalindrom (i + 1)
    isPalindrom 0
    
let firstTest = isPalindrom "otto"
let secondTest = isPalindrom "ttto"
let thirdTest = isPalindrom ""
let fourthTest = isPalindrom "w"
