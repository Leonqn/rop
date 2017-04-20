[<AutoOpen>]
module Rop

module Async = 

    let bind f x = async.Bind(x, f)
    
    let map f x = async.Bind(x, f >> async.Return)

module AsyncResult = 
    
    let retn x = Ok x |> async.Return

    let either successFunc failureFunc xAsyncResult = 
        async {
            let! xResult = xAsyncResult
            match xResult with
            | Ok s -> return! successFunc s
            | Error f -> return! failureFunc f
        }

    let bind f = either f (Failure >> async.Return)
    
    let map f = Async.map <| Result.map f
    
    let mapError f x = Async.map (Result.mapError f) x

    let fromAsync xAsync = Async.map Ok xAsync

type ResultBuilder() = 
    member __.Return value = AsyncResult.retn value
        
    member __.ReturnFrom asyncResult = asyncResult
        
    member __.Zero () = AsyncResult.retn ()
        
    member __.Delay generator = generator >> AsyncResult.fromAsync
            
    member __.Bind(asyncResult, binder) = AsyncResult.bind binder asyncResult
            
    member __.Bind(result, binder) = result |> async.Return |> AsyncResult.bind binder
        
    member __.TryWith(asyncResult, catchHandler) = AsyncResult.retn <| async.TryWith(asyncResult, catchHandler)

    member __.TryFinally(asyncResult, compensation) = AsyncResult.retn <| async.TryFinally(asyncResult, compensation)
        
    member __.Using(resource, binder) = AsyncResult.retn <| async.Using(resource, binder)
    
let result = ResultBuilder()