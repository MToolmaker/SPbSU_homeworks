module checkCorrectness
    type Direction =
        | Forward
        | Backward
    type BraceType =
        | Parenthesis
        | SquareBracket
        | Brace

    let braceChecker (s : string) =

        let toBrace char =
            match char with
            | '(' -> Some(Parenthesis, Forward)
            | ')' -> Some(Parenthesis, Backward)
            | '[' -> Some(SquareBracket, Forward)
            | ']' -> Some(SquareBracket, Backward)
            | '{' -> Some(Brace, Forward)
            | '}' -> Some(Brace, Backward)
            | _ -> None

            
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