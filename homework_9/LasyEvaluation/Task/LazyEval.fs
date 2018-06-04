module LazyEval
    open ILazy
    open System.Threading

    type SingleThreadedLazy<'T>(supplier : unit -> 'T) =
        let mutable value = Unchecked.defaultof<'T>
        let mutable evalulated = false
        
        interface ILazyEval<'T> with
            member __.Get() =
                if not evalulated then
                    value <- supplier()
                    evalulated <- true
                value
   
    type MultiThreadedLazy<'T>(supplier : unit -> 'T) = 
        let mutable value = Unchecked.defaultof<'T>
        let mutable evalulated = false  
        let mutable locker = new obj();

        interface ILazyEval<'T> with
            member __.Get() =
                Monitor.Enter locker
                try
                    if not evalulated then
                        value <- supplier()
                        evalulated <- true
                finally
                    Monitor.Exit locker
                value
    
    type LockFreeLazy<'T when 'T : not struct>(supplier: unit -> 'T) =
        let refValue = ref Unchecked.defaultof<'T>
        let mutable evalulated = false

        interface ILazyEval<'T> with
            member __.Get() =
                if not evalulated then
                    let v = supplier()

                    Interlocked.CompareExchange<'T>(refValue, v, Unchecked.defaultof<'T>) |> ignore
                    evalulated <- true
                refValue.Value

