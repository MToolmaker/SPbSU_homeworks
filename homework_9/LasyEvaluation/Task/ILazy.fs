module ILazy
    type ILazyEval<'T> =
        abstract member Get: unit -> 'T
