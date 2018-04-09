module checkCorrectness

    type Direction =
        | Forward
        | Backward
    type BraceType =
        | Parenthesis
        | SquareBracket
        | CurvyBracket

    let braceChecker (s : string) =
        let length = s.Length
        let toBrace char =
            match char with
            | '(' -> Some(Parenthesis, Forward)
            | ')' -> Some(Parenthesis, Backward)
            | '[' -> Some(SquareBracket, Forward)
            | ']' -> Some(SquareBracket, Backward)
            | '{' -> Some(CurvyBracket, Forward)
            | '}' -> Some(CurvyBracket, Backward)
            | _ -> None

        let rec bracketCheck index (list : BraceType list) =
            if index = length then
                if List.isEmpty list then true
                else false
            else
                let brace = toBrace s.[index]
                match brace with
                | Some(a, b) when b = Backward && (List.isEmpty list || a <> List.head list) -> false
                | Some(_, b) when b = Backward -> bracketCheck (index + 1) (List.tail list)
                | Some(a, _) -> bracketCheck (index + 1) (a :: list)
                | None -> bracketCheck (index + 1) list
        bracketCheck 0 []