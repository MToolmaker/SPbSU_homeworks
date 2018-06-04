module LazyFactory
    open ILazy
    open LazyEval
    
    type LazyFactory =
        static member CreateSingleThreadedLazy(supplier : unit -> 'T) =
            new SingleThreadedLazy<'T>(supplier) :> ILazyEval<'T>

        static member CreateMultiThreadedLazy(supplier : unit -> 'T) =
            new MultiThreadedLazy<'T>(supplier) :> ILazyEval<'T>

        static member CreateLockFreeLazy(supplier : unit -> 'T when 'T : not struct) =
            new LockFreeLazy<'T>(supplier) :> ILazyEval<'T>