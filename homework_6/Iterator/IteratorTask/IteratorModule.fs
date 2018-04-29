module IteratorModule
    open System.Collections.Generic
    //Immutable binary search tree
    type Tree<'key, 'value> =
        | Empty
        | Node of value: ('key * 'value) * left: Tree<'key, 'value> * right: Tree<'key, 'value>
        interface IEnumerable<'value> with
            member this.GetEnumerator(): IEnumerator<'value> = 
                new TreeEnum<'key, 'value>(this) :> IEnumerator<'value>
            member this.GetEnumerator(): System.Collections.IEnumerator = 
                new TreeEnum<'key, 'value>(this) :> System.Collections.IEnumerator
    
    and TreeEnum<'key, 'value>(tree : Tree<'key, ' value>) =

        let rec linearize (binTree : Tree<'key, 'value>) =
            match binTree with
            | Empty -> []
            | Node((_, value), left, right) -> (linearize left) @ [value] @ (linearize right)
        
        let values = linearize tree
        let mutable position = -1
        let length = List.length values

        interface IEnumerator<'value> with
            member this.Current: 'value =
                if position < length then
                    List.item position values
                else
                    raise <| System.InvalidOperationException()

            member this.Current: obj = box (this :> IEnumerator<'value>).Current
                
            member __.Dispose(): unit =  ()

            member __.MoveNext(): bool =
                position <- position + 1
                position < length

            member __.Reset(): unit =
                position <- -1


    let isBinarySearchTree (tree : Tree<'key, 'value>) =
        let rec IsBinarySearchTree leastKey highestKey tree =
            match tree with
            | Empty -> true
            | Node ((key, _), left, right) ->
                match leastKey, highestKey with
                | Some(leastKey), _ when key < leastKey -> false
                | _, Some(highestKey) when key > highestKey -> false
                | _ ->
                    let newHighestValue = Some(defaultArg highestKey key |> min key)
                    let newLeastValue = Some(defaultArg leastKey key |> max key)
                    IsBinarySearchTree leastKey newHighestValue left && IsBinarySearchTree newLeastValue highestKey right
        IsBinarySearchTree None None tree
    
    let rec find key (tree : Tree<'a, 'b>) =
        match tree with
        | Empty -> None
        | Node((key', value), _, _) when key = key' -> Some(value)
        | Node((key', _), left, _) when key < key' -> find key left
        | Node((key', _), _, right) when key > key' -> find key right

    let rec insert newKey newValue (tree : Tree<'key, 'value>) =
        match tree with
        | Empty -> Node ((newKey, newValue), Empty, Empty)
        | Node ((key, value), left, right) when newKey < key ->
            let newLeft = insert newKey newValue left
            Node ((key, value), newLeft, right)
        | Node ((key, value), left, right) when newKey > key->
            let newRight = insert newKey newValue right
            Node ((key, value), left, newRight)
        | _ -> raise <| System.ArgumentException("Given key already exists.")
   
    let rec delete key (tree : Tree<'a, 'b>) =
        let rec findInOrderPredecessor (tree : Tree<'a, 'b>) =
            match tree with
            | Empty -> Empty
            | Node (_, _, Empty) -> tree
            | Node (_, _, right) -> findInOrderPredecessor right

        match tree with
        | Empty -> Empty
        | Node ((key', value'), left, right) when key < key' ->
            let left' = delete key left
            Node ((key', value'), left', right)
        | Node ((key', value'), left, right) when key > key' ->
            let right' = delete key right
            Node ((key', value'), left, right')
        | Node (_, Empty, Empty) -> Empty
        | Node (_, left, Empty) -> left
        | Node (_, Empty, right) -> right
        | Node (_, left, right) ->
            let (Node((key', value'), _, _)) = findInOrderPredecessor left
            let left' = delete key' left
            Node ((key', value'), left', right)

