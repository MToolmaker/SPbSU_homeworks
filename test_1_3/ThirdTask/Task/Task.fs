module TaskModule
    type Hashtable<'TKey, 'TValue when 'TValue : equality>(size : int, getHash : 'TKey -> int) =
        let mutable array : 'TValue list [] = Array.zeroCreate size
        do for i in 0 .. size-1 do array.[i] <- []
            
        member __.Add(key : 'TKey) (value : 'TValue) =
            let index = (getHash key) % size
            let list = array.[index]
            if List.exists (fun x -> x = value) list then
                raise <| System.ArgumentException("Given value already exists.")
            array.[index] <- value :: list
            
        member __.Contains(key : 'TKey) (value : 'TValue) = List.exists (fun x -> x = value) array.[(getHash key) % size]

        member __.Delete(key : 'TKey) (value : 'TValue) =
            array.[(getHash key) % size] <- List.filter (fun x -> x <> value) array.[(getHash key) % size]
            
                
        
