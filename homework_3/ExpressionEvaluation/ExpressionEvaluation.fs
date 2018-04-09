module ExpressionEvaluation
    type ExpressionTree =
        | Value of int 
        | Add of ExpressionTree * ExpressionTree
        | Subtract of ExpressionTree * ExpressionTree
        | Multiply of ExpressionTree * ExpressionTree
        | Divide of ExpressionTree * ExpressionTree
    
    let rec eval (tree : ExpressionTree) =
        match tree with
            | Value(x) -> x
            | Add(x, y) -> eval(x) + eval(y)
            | Subtract(x, y) -> eval(x) - eval(y)
            | Multiply(x, y) -> eval(x) * eval(y)
            | Divide(x, y) ->
                if eval(y) = 0 then
                    failwith "Dividing by zero"
                else if eval(x) % eval(y) <> 0 then
                    failwith "Non-integer dividing"
                else
                    eval(x) / eval(y)
