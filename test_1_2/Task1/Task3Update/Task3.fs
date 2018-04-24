module Task3
    type PriorityQueue<'T>() =
        let mutable queue : (int * 'T) list = []
        member this.insert priority value = queue <- (priority, value) :: queue
        member this.extractMin () =
            let mutable min = None
            let rec find list =
                match list with
                | [] -> ()
                | list -> if min = None || (fst <| List.head list ) < (fst min.Value) then min <- Some(List.head list)
            find queue
            snd min.Value


