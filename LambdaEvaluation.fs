module LambdaEvaluation
    type Term =
        | Variable of string
        | Abstr of string * Term
        | App of Term * Term

    let rec betaReduce (expr : Term) =
        let rec isNotAbstr (expr : Term) =
            match expr with
            | Abstr(_, _) -> false
            | _ -> true
        and isNormalForm (expr : Term) =
            match expr with
            | _ when isNormalFormNotAbstr expr -> true
            | Abstr(_, b) when isNormalForm b -> true
            | _ -> false
        and isNormalFormNotAbstr (expr : Term) =
            match expr with
            | Variable(_) -> true
            | App(a, b) when (isNormalFormNotAbstr a && isNormalForm b) -> true
            | _ -> false

        let renameBoundVarsAndSubstitute (boundVar : string) (abstrBody : Term) (term : Term) =
            let substitute (boundVar : string) (abstrBody : Term) (term : Term) =
                let rec substitute innerTerm =
                    match innerTerm with
                    | Variable(a) when a = boundVar -> term
                    | Variable(_) -> innerTerm
                    | App(a, b) -> App(substitute a, substitute b)
                    | Abstr(a,b) -> Abstr(a, substitute b)
                substitute abstrBody
            let renameBoundVars (abstrBody : Term) (term : Term) =
                let removeDuplicates list1 =
                    let rec isDuplicate list x =
                        match list with
                        | [] -> false
                        | head :: tail -> if x = head then true else isDuplicate tail x
            
                    let rec removeDuplicates list1 list2 =
                        match list1 with
                        | [] -> list2
                        | head :: tail when isDuplicate list2 head = false -> removeDuplicates tail (head::list2)
                        | _::t -> removeDuplicates t list2
                    removeDuplicates list1 []
                let findFV (term : Term) =
                    let rec findFV FV BV (term : Term) =
                        match term with
                        | Variable(a) when List.contains a BV = false  -> a :: FV
                        | Variable(_) -> FV
                        | App(a,b) -> (findFV FV BV a)  @ (findFV FV BV b)
                        | Abstr(a,b) -> findFV FV (a :: BV) b
                    findFV [] [] term |> removeDuplicates
                let FV = findFV term
                let BV = boundVar :: []
                let rec getNewName currentName bodyFV =
                    if List.contains (currentName + "'") FV = false && List.contains (currentName + "'") bodyFV = false then
                        currentName + "'"
                    else
                        getNewName (currentName + "'") bodyFV
                //Abstr(a,b) when List.contains a FV -> Abstr(getNewName a (findFV b), substitute (a b (Variable(getNewName a (findFV b))))
                let rec renameBoundVars BV term =
                    match term with
                    | Variable(a) -> Variable(a) 
                    | App(a,b) -> App(renameBoundVars BV a, renameBoundVars BV b)
                    | Abstr(a,b) when List.contains a FV  -> (Abstr(getNewName a (findFV b), renameBoundVars (getNewName a (findFV b) :: BV) (substitute a b (Variable(getNewName a (findFV b))))))
                    | Abstr(a,b) -> Abstr(a, renameBoundVars (a::BV) b)
                renameBoundVars BV abstrBody

            //После переименования, можно подставлять выражение term зная, что свободные переменные term имеют отличные названия от связанных в abstrBody
            substitute boundVar (renameBoundVars abstrBody term) term
        
        if isNormalForm expr then expr
        else
            //Ссылаясь ну курс Дениса Москвина, выполняется ровно одно из следующих правил бета редукции по нормальной стратегии, поэтому данный match обрабатывает всевозможные случаи.
            match expr with
            | Abstr(a, b) -> Abstr(a, betaReduce b)
            | App(a, b) when isNotAbstr a -> betaReduce (App(betaReduce a,b))     
            | App(a, b) when isNormalFormNotAbstr a -> betaReduce (App(a, betaReduce b))
            | App(Abstr(boundVar, abstrBody), term) -> betaReduce (renameBoundVarsAndSubstitute boundVar abstrBody term)


