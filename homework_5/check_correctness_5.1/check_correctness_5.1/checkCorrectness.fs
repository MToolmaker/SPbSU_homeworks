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
                | Some(a, b) when b = Backward -> bracketCheck (index + 1) (List.tail list)
                | Some(a, b) -> bracketCheck (index + 1) (a :: list)
                | None -> bracketCheck (index + 1) list
        bracketCheck 0 []
                

                
        (*
        let rec braceChecker beginIndex endIndex currentBrace direction =
            if beginIndex > endIndex then
                if currentBrace = None then
                    true
                else
                    false
            else
                if direction = Forward then
                    let brace = toBrace (s.[beginIndex])

                    match brace with
                    | Some(_) when snd brace.Value = Backward -> false
                    | Some(_) -> braceChecker beginIndex endIndex brace Backward
                    | _ -> braceChecker (beginIndex + 1) endIndex None Forward
                else
                    let brace = toBrace (s.[endIndex])
                    
                    match brace with
                    | Some(_) when Some(fst currentBrace.Value, Backward) = brace -> braceChecker (beginIndex + 1) (endIndex - 1) None Forward
                    | Some(_) -> false
                    | None -> braceChecker beginIndex (endIndex - 1) currentBrace Backward
        braceChecker 0 (s.Length - 1) None Forward
        *)